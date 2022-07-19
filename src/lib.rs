extern crate proc_macro;
use quote::quote;
use syn::parse_macro_input;
use syn::Data;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Fields;

use self::proc_macro::TokenStream;

/// 2 -> ( $1,$2 )
fn dollar_values(max: usize) -> String {
    let itr = 1..max + 1;
    itr.into_iter()
        .map(|s| format!("${}", s))
        .collect::<Vec<String>>()
        .join(",")
}

/// Create method for inserting struts into Sqlite database
///
/// ```rust
/// # #[tokio::main]
/// # async fn main() -> eyre::Result<()>{
/// #[derive(Default, Debug, sqlx::FromRow, sqlxinsert::SqliteInsert)]
/// struct Car {
///     pub car_id: i32,
///     pub car_name: String,
/// }
///
/// let car = Car {
///     car_id: 33,
///     car_name: "Skoda".to_string(),
/// };
///
/// let url = "sqlite::memory:";
/// let pool = sqlx::sqlite::SqlitePoolOptions::new().connect(url).await.unwrap();
///
/// let create_table = "create table cars ( car_id INTEGER PRIMARY KEY, car_name TEXT NOT NULL )";
/// sqlx::query(create_table).execute(&pool).await.expect("Not possible to execute");
///
/// let res = car.insert_raw(&pool, "cars").await.unwrap(); // returning id
/// # Ok(())
/// # }
/// ```
///
#[proc_macro_derive(SqliteInsert)]
pub fn derive_from_struct_sqlite(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };

    // Attributes -> field names
    let field_name = fields.iter().map(|field| &field.ident);
    let field_name2 = fields.iter().map(|field| &field.ident);

    let struct_name = &input.ident;

    let field_length = field_name.len();
    // ( $1, $2)
    let values = dollar_values(field_length);

    let fields_list = quote! {
        #( #field_name ),*
    };
    let columns = format!("{}", fields_list);

    TokenStream::from(quote! {

        impl #struct_name {
            pub fn insert_query(&self, table: &str) -> String
            {
                let sqlquery = format!("insert into {} ( {} ) values ( {} ) returning *", table, #columns, #values); //self.values );
                sqlquery
            }

            pub async fn insert_raw(&self, pool: &sqlx::SqlitePool, table: &str) -> eyre::Result<sqlx::sqlite::SqliteQueryResult>
            {
                let sql = self.insert_query(table);
                Ok(sqlx::query(&sql)
                #(
                    .bind(&self.#field_name2)//         let #field_name: #field_type = Default::default();
                )*
                    .execute(pool)// (&mut conn)
                    .await?
                )
            }
        }
    })
}

/// Create method for inserting struts into Postgres database
///
/// ```rust,ignore
/// # #[tokio::main]
/// # async fn main() -> eyre::Result<()>{
///
/// #[derive(Default, Debug, std::cmp::PartialEq, sqlx::FromRow)]
/// struct Car {
///     pub id: i32,
///     pub name: String,
/// }
///
/// #[derive(Default, Debug, sqlx::FromRow, sqlxinsert::PgInsert)]
/// struct CreateCar {
///     pub name: String,
///     pub color: Option<String>,
/// }
/// impl CreateCar {
///     pub fn new<T: Into<String>>(name: T) -> Self {
///         CreateCar {
///             name: name.into(),
///             color: None,
///         }
///     }
/// }
/// let url = "postgres://user:pass@localhost:5432/test_db";
/// let pool = sqlx::postgres::PgPoolOptions::new().connect(&url).await.unwrap();
///
/// let car_skoda = CreateCar::new("Skoda");
/// let res: Car = car_skoda.insert::<Car>(pool, "cars").await?;
/// # Ok(())
/// # }
/// ```
///
#[proc_macro_derive(PgInsert)]
pub fn derive_from_struct_psql(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };
    let field_name = fields.iter().map(|field| &field.ident);
    let field_name_values = fields.iter().map(|field| &field.ident);

    let field_length = field_name.len();
    // struct Car { id: i32, name: String }
    // -> ( $1,$2 )
    let values = dollar_values(field_length);

    // struct Car ...
    // -> Car
    let struct_name = &input.ident;

    // struct { id: i32, name: String }
    // -> ( id, name )
    let columns = format!(
        "{}",
        quote! {
            #( #field_name ),*
        }
    );

    TokenStream::from(quote! {
        impl #struct_name {
            fn insert_query(&self, table: &str) -> String
            {
                let sqlquery = format!("insert into {} ( {} ) values ( {} ) returning *", table, #columns, #values); // self.value_list()); //self.values );
                sqlquery
            }

            pub async fn insert<'c, T, E>(&self, executor: E, table: &str) -> sqlx::Result<T>
            where
                T: Send,
                T: for<'r> sqlx::FromRow<'r, sqlx::postgres::PgRow>,
                T: std::marker::Unpin,
                E: sqlx::Executor<'c, Database = sqlx::Postgres>,
            {
                let sql = self.insert_query(table);
                // let mut pool = pool;
                let res: T = sqlx::query_as::<_,T>(&sql)
                #(
                    .bind(&self.#field_name_values)
                )*
                    .fetch_one(executor)
                    .await?;

                Ok(res)
            }

        }
    })
}

#[proc_macro_derive(PgUpdate)]
pub fn derive_pg_update_from_struct_psql(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };

    // Create two iterators as we need to iterate through the fields twice
    let field_name_iter = fields.iter().map(|field| &field.ident);
    let field_name_iter_for_values = fields.iter().map(|field| &field.ident);
    let struct_name = &input.ident;

    TokenStream::from(quote! {
        impl #struct_name {
            pub fn update_query(&self, table: &str) -> String
            {
                let mut set_expressions = Vec::new();
                let mut ix = 1;
                #(
                    match &self.#field_name_iter {
                        Some(value) => {
                            let field_name_str = stringify!(#field_name_iter);
                            let expr = format!("{} = {}", field_name_str, format!("${}", ix));
                            set_expressions.push(expr);
                            ix += 1;
                        },
                        None => ()
                    }
                )*

                let set_expression = set_expressions.join(", ");
                let query = format!("UPDATE {} SET {} RETURNING *", table, set_expression);
                query
            }

            pub async fn update<'c, T, E>(&self, executor: E, table: &str) -> sqlx::Result<T>
            where
                T: Send,
                T: for<'r> sqlx::FromRow<'r, sqlx::postgres::PgRow>,
                T: std::marker::Unpin,
                E: sqlx::Executor<'c, Database = sqlx::Postgres>,
            {
                let sql = self.update_query(table);
                let mut query = sqlx::query_as::<_,T>(&sql);
                #(
                    match &self.#field_name_iter_for_values {
                        Some(value) => {
                            query = query.bind(value);
                        }
                        None => ()
                    }
                )*
                let res: T = query
                    .fetch_one(executor)
                    .await?;
                Ok(res)
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn range_test() {
        let itr = 1..4;
        let res = itr
            .into_iter()
            .map(|s| format!("${}", s))
            .collect::<Vec<String>>()
            .join(",");

        assert_eq!(res, "$1,$2,$3");
    }

    #[test]
    fn dollar_value_tes() {
        let res = dollar_values(3);
        assert_eq!(res, "$1,$2,$3");
    }
}
