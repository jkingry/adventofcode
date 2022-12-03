
pub fn parse_number(line: &[u8]) -> i32 {
    let mut n: i32 = 0;
    for b in line.iter() {
        n = n * 10 + i32::from(b - b'0')
    }
    n
}

