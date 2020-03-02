extern crate csv;
//#[macro_use]
extern crate serde_derive;
extern crate walkdir;
extern crate sha2;
extern crate data_encoding;
extern crate ring;
extern crate mpi;
extern crate reqwest;
extern crate select;
//extern crate clang;
extern crate kmpsearch;

//use std::io;
//use std::io::prelude::*;
//use std::fs::File;
//use csv::Error;
//use std::process;
use mpi::traits::*;
use mpi::request::WaitGuard;
use mpi::topology::{Rank};

#[warn(unused_imports)]
use walkdir::{DirEntry, WalkDir};
use std::env;
use std::fs;
use csv::ReaderBuilder;
use csv::Error;
///use crate::Haystack;

pub mod util;
pub mod scrape;
//pub mod ast_cpp;

//#[derive(Deserialize)]
//struct Record {
//    id: String,
//    name: String,
//    repo: String,
//}

//fn example() -> Result<(), Box<Error>> {
//     let data = 
//     let mut rdr = ReaderBuilder::new()
//         .delimiter(b';')
//         .from_reader(data.as_bytes());
//
//     if let Some(result) = rdr.records().next() {
//         let record = result?;
//         assert_eq!(record, vec!["Boston", "United States", "4628910"]);
//         Ok(())
//     } else {
//         Err(From::from("expected at least one record but got none"))
//     }
// }

#[warn(dead_code)]
fn is_not_hidden(entry: &DirEntry) -> bool {
    entry
         .file_name()
         .to_str()
         .map(|s| entry.depth() == 0 || !s.starts_with("."))
         .unwrap_or(false)
}

fn main() {
    let universe = mpi::initialize().unwrap();
    let world = universe.world();
    let size = world.size();
    let rank = world.rank();

    if size != 2 {
        panic!("Size of MPI_COMM_WORLD must be 2, but is {}!", size);
     }

    let args: Vec<String>  = env::args().collect();
    //let file1 = &args[1];
    //let file2 = &args[2];
    let file  = String::from("../data/repo-pairs.csv");
    let pairs = util::runcsv(file);

    match rank {
        0 => {

        }
    }
    //println!("{:?}", pairs.len());
    //for r in pairs {
    //    println!("{:?}", r);
    //}

    //let tree1 = fs::read_to_string(file1)
    //    .expect("No file for tree 1.");
    //let tree2 = fs::read_to_string(file2)
    //    .expect("No file for tree 2.");
}
