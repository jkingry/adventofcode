use std::{error::Error, io::{self}};
use bstr::{io::BufReadExt};

fn find_common(a: &[u8], b: &[u8]) -> u8 {
    for item1 in a {
        for item2 in b {
            if item1 == item2 {
                return item1.to_owned();
            }
        }
    }

    return 0;
}

fn item_score(c: u8) -> i32 {
    let score = 
        c - 
        if b'a' <= c && c <= b'z' { b'a' - 1} else { b'A' - 27 };

    i32::from(score)
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();

    let mut part1 = 0;

    stdin.lock().for_byte_line(|line| {
        let mid = line.len() / 2;

        let sack1 = &line[..mid];
        let sack2 = &line[mid..];
        
        let c = find_common(sack1, sack2);

        part1 += item_score(c);

        Ok(true)
    })?;
    
    println!("Day 03 - Part 1: {}", part1);
    println!("Day 03 - Part 2: {}", 0);

    Ok(())
}