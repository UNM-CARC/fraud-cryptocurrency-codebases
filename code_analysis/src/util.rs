use std::io::{BufReader, Read};
//use std::io::prelude::*;
use std::fs::File;
//use std::string::ToString;
use csv::Error;
//use std::process;
use walkdir::DirEntry;
//use sha2::{Sha256, Digest};
use data_encoding::HEXUPPER;
use ring::digest::{Context, Digest, SHA256};

type Record = (String, String, String);

pub fn runcsv() -> Result<(), Error> {
    let file = File::open("names.csv")?;
    let mut reader = csv::Reader::from_reader(file);
    for result in reader.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

pub fn sha256_digest<R: Read>(mut reader: R) -> Result<Digest, Error> {
    let mut context = Context::new(&SHA256);
    let mut buffer = [0; 1024];

    loop {
        let count = reader.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        context.update(&buffer[..count]);
    }

    Ok(context.finish())
}

pub fn runhash(filename: DirEntry) -> Result<(), Error> {
    let mut file = File::open(filename.path())?;
    let reader = BufReader::new(file);
    let digest = sha256_digest(reader)?;
    //let mut sha256 = Sha256::new();
    //io::copy(&mut file, &mut sha256)?;
    //let hash = sha256.result();
    //return hash;
    println!("SHA-256 digest is {}", HEXUPPER.encode(digest.as_ref()));
    //return digest.as_ref();
    Ok(())
}
