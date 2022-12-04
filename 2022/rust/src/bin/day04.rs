use std::{error::Error, io::{self}};
use bstr::{io::BufReadExt};
use aoc_2022::*;

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();

    let mut part1 = 0;
    let mut part2 = 0;

    stdin.lock().for_byte_line(|line| {
        let mut parts = line.split(|c| { *c == b',' || *c == b'-' });

        let a1 = parse_number(parts.next().unwrap());
        let a2 = parse_number(parts.next().unwrap());
        let b1 = parse_number(parts.next().unwrap());
        let b2 = parse_number(parts.next().unwrap());

        if (a1 <= b1 && b2 <= a2) || (b1 <= a1 && a2 <= b2) {
            part1 += 1;
        }

        if a1 <= b2 && a2 >= b1 {
            part2 += 1;
        }

        Ok(true)
    })?;
    
    println!("Day 04 - Part 1: {}", part1);
    println!("Day 04 - Part 2: {}", part2);

    Ok(())
}