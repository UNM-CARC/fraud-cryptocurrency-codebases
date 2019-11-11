extern crate csv;
//#[macro_use]
extern crate serde_derive;
extern crate walkdir;
extern crate sha2;
extern crate data_encoding;
extern crate ring;
//extern crate mpi;
extern crate reqwest;
extern crate select;
extern crate clang;
extern crate kmpsearch;

//use std::io;
//use std::io::prelude::*;
//use std::fs::File;
//use csv::Error;
//use std::process;
//use mpi::traits::*;
//use mpi::request::WaitGuard;

#[warn(unused_imports)]
use walkdir::{DirEntry, WalkDir};
use std::env;
use std::fs;
///use crate::Haystack;

pub mod util;
pub mod scrape;
pub mod ast_cpp;

//#[derive(Deserialize)]
//struct Record {
//    id: String,
//    name: String,
//    repo: String,
//}

#[warn(dead_code)]
fn is_not_hidden(entry: &DirEntry) -> bool {
    entry
         .file_name()
         .to_str()
         .map(|s| entry.depth() == 0 || !s.starts_with("."))
         .unwrap_or(false)
}

fn main() {
    //let universe = mpi::initialize().unwrap();
    //let world = universe.world();
    //let size = world.size();
    //let rank = world.rank();

    //scrape::scraper("https://coinmarketcap.com/all/views/all/")

    //if let Err(err) = util::runcsv() {
    //    println!("{}", err);
    //    process::exit(1);
    //}
    //WalkDir::new("/tmp/BTC")
    //    .into_iter()
    //    .filter_entry(|e| is_not_hidden(e))
    //    .filter_map(|v| v.ok())
    //    .for_each(|x| println!("{}", x.path().display()));

    //WalkDir::new("/tmp/BTC")
    //    .into_iter()
    //    .filter_entry(|e| is_not_hidden(e))
    //    .filter_map(|y| util::runhash(y))
    //    .filter_map(|v| v.ok())
    //    .for_each(|x| println!("{}", x.path().display()));
    //ast_cpp::parsecpp();
    let args: Vec<String>  = env::args().collect();
    let file1 = &args[1];
    let file2 = &args[2];
    let tree1 = fs::read_to_string(file1)
        .expect("No file for tree 1.");
    let tree2 = fs::read_to_string(file2)
        .expect("No file for tree 2.");
}
