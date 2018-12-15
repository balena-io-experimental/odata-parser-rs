#![recursion_limit="128"]
#[macro_use] extern crate nom;

mod parser;
mod ast;

fn main() {
	// println!("{:?}", parser::odataRelativeUri("ProductsByComplex(complex=@c)?@c={\"@odata.type\":\"Model.Customer\",\"Name\":\"Value\"}\n"));
	println!("{:?}", parser::odataRelativeUri("$batch?$format=application/json\n"));
}
