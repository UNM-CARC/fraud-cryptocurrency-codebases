extern crate csv;
#[macro_use]
extern crate serde_derive;

//use std::io;
use std::io::prelude::*;
use std::fs::File;
use csv::Error;
use std::process;

//#[derive(Deserialize)]
//struct Record {
//    id: String,
//    name: String,
//    repo: String,
//}

type Record = (String, String, String);

fn run() -> Result<(), Error> {
    let file = File::open("names.csv")?;
    //let mut buff = [0;1024];
    //let csv      = f.read(&mut buff)?;

    let mut reader = csv::Reader::from_reader(file);
    for result in reader.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = run() {
        println!("{}", err);
        process::exit(1);
    }
}
