extern crate csv;
//#[macro_use]
extern crate serde_derive;
extern crate walkdir;
extern crate sha2;
extern crate data_encoding;
extern crate ring;

//use std::io;
//use std::io::prelude::*;
//use std::fs::File;
//use csv::Error;
//use std::process;
use walkdir::{DirEntry, WalkDir};

mod util;

//#[derive(Deserialize)]
//struct Record {
//    id: String,
//    name: String,
//    repo: String,
//}
fn is_not_hidden(entry: &DirEntry) -> bool {
    entry
         .file_name()
         .to_str()
         .map(|s| entry.depth() == 0 || !s.starts_with("."))
         .unwrap_or(false)
}

fn main() {
    //if let Err(err) = util::runcsv() {
    //    println!("{}", err);
    //    process::exit(1);
    //}
    //WalkDir::new("/tmp/BTC")
    //    .into_iter()
    //    .filter_entry(|e| is_not_hidden(e))
    //    .filter_map(|v| v.ok())
    //    .for_each(|x| println!("{}", x.path().display()));

    WalkDir::new("/tmp/BTC")
        .into_iter()
        .filter_entry(|e| is_not_hidden(e))
        .filter_map(|y| util::runhash(y))
        .filter_map(|v| v.ok())
        .for_each(|x| println!("{}", x.path().display()));
}
