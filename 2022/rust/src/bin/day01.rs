use std::{error::Error, io::{self}};
use bstr::{io::BufReadExt};

use aoc_2022::*;

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();

    let mut top3 = [0, 0, 0];
    let mut sum = 0;

    stdin.lock().for_byte_line(|line| {
        if line.len() == 0 {
            for i in 0..3 { 
                if sum > top3[i] {
                    top3[i] = sum;
                    break;
                }
            }
            sum = 0;
        } else {
            sum += parse_number(line);            
        }        
        Ok(true)
    })?;
    
    println!("Day 01 - Part 1: {}", top3[0]);
    println!("Day 01 - Part 2: {}", top3[0] + top3[1] + top3[2]);

    Ok(())
}