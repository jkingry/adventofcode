use std::{error::Error, io::{self}};
use bstr::{io::BufReadExt};

fn find_common_2(a: &[u8], b: &[u8]) -> u8 {
    for item1 in a {
        for item2 in b {
            if item1 == item2 {
                return item1.to_owned();
            }
        }
    }

    return 0;
}

fn find_common_3(a: &[u8], b: &[u8], c: &[u8]) -> u8 {
    for item1 in a {
        for item2 in b {
            if item1 == item2 {
                for item3 in c {
                    if item1 == item3 {
                        return item1.to_owned();
                    }
                }
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

    let mut part2 = 0;

    let mut group_count = 0;
    let mut line1 = [0u8; 50]; 
    let mut line1_len = 0;
    let mut line2 = [0u8; 50]; 
    let mut line2_len = 0;

    stdin.lock().for_byte_line(|line| {
        let mid = line.len() / 2;

        let sack1 = &line[..mid];
        let sack2 = &line[mid..];
        
        let c = find_common_2(sack1, sack2);

        part1 += item_score(c);
 
        match group_count {
            0 => {
                line1_len = line.len();
                line1[..line1_len].copy_from_slice(line);
            },
            1 => {
                line2_len = line.len();
                line2[..line2_len].copy_from_slice(line);
            },
            2 => { 
                let group_c = find_common_3(
                    &line1[..line1_len], 
                    &line2[..line2_len], 
                    line);
                part2 += item_score(group_c);
            },
            _ => panic!("wtf"),
        };
        group_count = (group_count + 1) % 3;

        Ok(true)
    })?;
    
    println!("Day 03 - Part 1: {}", part1);
    println!("Day 03 - Part 2: {}", part2);

    Ok(())
}
