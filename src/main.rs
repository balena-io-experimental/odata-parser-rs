#![recursion_limit="128"]
#[macro_use] extern crate nom;

mod parser;
mod ast;

fn main() {
	println!("{:?}", parser::odataRelativeUri("PrafdsdoductsByComplex(complex=@c)?@c={\"@odata.type\":\"Model.Customer\",\"Name\":\"Value\"}\n"));
}
