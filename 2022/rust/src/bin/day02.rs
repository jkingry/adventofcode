use std::{error::Error, io::{self}};
use bstr::{io::BufReadExt};

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();

    let mut part1 = 0;
    let mut part2 = 0;

    stdin.lock().for_byte_line(|line| {
        let them = i32::from(line[0] - b'A');
        let you = i32::from(line[2] - b'X');

        part1 += 1 + you + 
            if you == them { 3 }
            else if you == (them + 1) % 3 { 6 }
            else { 0 };
    
        part2 += 1 +
            if you == 0 { 0 + ((them + 2) % 3) }
            else if you == 1 { 3 + them }
            else { 6 + ((them + 1) % 3) };

        Ok(true)
    })?;
    
    println!("Day 02 - Part 1: {}", part1);
    println!("Day 02 - Part 2: {}", part2);

    Ok(())
}