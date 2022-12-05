for input in $(ls ../input) 
do
    day="day${input%.*}"
    cat ../input/$input | ./target/release/day
done
