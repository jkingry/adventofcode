namespace AdventOfCode.FSharp.Y2021

// Day 16: Packet Decoder
module Day16 =
    open AdventOfCode.FSharp.Util
    open System.Collections
    open System.Linq

    let reverse = 
        [|
            0x00; 0x80; 0x40; 0xc0; 0x20; 0xa0; 0x60; 0xe0;
            0x10; 0x90; 0x50; 0xd0; 0x30; 0xb0; 0x70; 0xf0;
            0x08; 0x88; 0x48; 0xc8; 0x28; 0xa8; 0x68; 0xe8;
            0x18; 0x98; 0x58; 0xd8; 0x38; 0xb8; 0x78; 0xf8;
            0x04; 0x84; 0x44; 0xc4; 0x24; 0xa4; 0x64; 0xe4;
            0x14; 0x94; 0x54; 0xd4; 0x34; 0xb4; 0x74; 0xf4;
            0x0c; 0x8c; 0x4c; 0xcc; 0x2c; 0xac; 0x6c; 0xec;
            0x1c; 0x9c; 0x5c; 0xdc; 0x3c; 0xbc; 0x7c; 0xfc;
            0x02; 0x82; 0x42; 0xc2; 0x22; 0xa2; 0x62; 0xe2;
            0x12; 0x92; 0x52; 0xd2; 0x32; 0xb2; 0x72; 0xf2;
            0x0a; 0x8a; 0x4a; 0xca; 0x2a; 0xaa; 0x6a; 0xea;
            0x1a; 0x9a; 0x5a; 0xda; 0x3a; 0xba; 0x7a; 0xfa;
            0x06; 0x86; 0x46; 0xc6; 0x26; 0xa6; 0x66; 0xe6;
            0x16; 0x96; 0x56; 0xd6; 0x36; 0xb6; 0x76; 0xf6;
            0x0e; 0x8e; 0x4e; 0xce; 0x2e; 0xae; 0x6e; 0xee;
            0x1e; 0x9e; 0x5e; 0xde; 0x3e; 0xbe; 0x7e; 0xfe;
            0x01; 0x81; 0x41; 0xc1; 0x21; 0xa1; 0x61; 0xe1;
            0x11; 0x91; 0x51; 0xd1; 0x31; 0xb1; 0x71; 0xf1;
            0x09; 0x89; 0x49; 0xc9; 0x29; 0xa9; 0x69; 0xe9;
            0x19; 0x99; 0x59; 0xd9; 0x39; 0xb9; 0x79; 0xf9;
            0x05; 0x85; 0x45; 0xc5; 0x25; 0xa5; 0x65; 0xe5;
            0x15; 0x95; 0x55; 0xd5; 0x35; 0xb5; 0x75; 0xf5;
            0x0d; 0x8d; 0x4d; 0xcd; 0x2d; 0xad; 0x6d; 0xed;
            0x1d; 0x9d; 0x5d; 0xdd; 0x3d; 0xbd; 0x7d; 0xfd;
            0x03; 0x83; 0x43; 0xc3; 0x23; 0xa3; 0x63; 0xe3;
            0x13; 0x93; 0x53; 0xd3; 0x33; 0xb3; 0x73; 0xf3;
            0x0b; 0x8b; 0x4b; 0xcb; 0x2b; 0xab; 0x6b; 0xeb;
            0x1b; 0x9b; 0x5b; 0xdb; 0x3b; 0xbb; 0x7b; 0xfb;
            0x07; 0x87; 0x47; 0xc7; 0x27; 0xa7; 0x67; 0xe7;
            0x17; 0x97; 0x57; 0xd7; 0x37; 0xb7; 0x77; 0xf7;
            0x0f; 0x8f; 0x4f; 0xcf; 0x2f; 0xaf; 0x6f; 0xef;
            0x1f; 0x9f; 0x5f; 0xdf; 0x3f; 0xbf; 0x7f; 0xff 
        |] |> Array.map byte

    type BitStream(input: BitArray) = 
        let mutable pos = -1
        let data = input.Cast<bool> () |> Seq.toArray
        
        member _.eof = pos >= data.Length - 1
        member _.read1 () =
            pos <- pos + 1
            data.[pos]
        member this.read n =
            let mutable p = 0
            seq {
                while p < n && not this.eof do
                    yield this.read1 ()
                    p <- p + 1
            }
        member this.subStream n =
            let input = (this.read n) |> Seq.toArray
            let a = new BitArray (input)
            new BitStream (a)
        static member parse (text : string) =
            let a =
                System.Convert.FromHexString text 
                |> Array.map (fun b -> reverse.[int b])
                |> BitArray
            new BitStream(a)

    let bits (length: int) (s : BitStream) =
        let mutable v = 0L
        for bit in s.read length do
            v <- (v <<< 1) + (if bit then 1L else 0L)
        v

    let readLiteral (s : BitStream) = 
        let mutable v = 0L
        let mutable keepReading = true
        while keepReading do
            keepReading <- s.read1 ()
            let a = bits 4 s            
            v <- (v <<< 4) + a
        v
    type PacketValue =
        | Op of Packet list
        | L of int64        
    and Packet =
        {
            version:int64
            typeid:int64
            value:PacketValue
        }
        
    let rec parsePacket stream=

        let version = bits 3 stream
        let typeid =  bits 3 stream

        if typeid = 4 then
            let value = readLiteral stream     
            { version = version; typeid = typeid; value = L value }
        else
            let lengthTypeID = stream.read1 ()
            let mutable packets = []
            if lengthTypeID then
                let packetLength = bits 11 stream |> int
                for _ = 1 to packetLength do
                    packets <- packets @ [ parsePacket stream ]
            else
                let bitLength = bits 15 stream |> int
                let sub = stream.subStream bitLength
                while not sub.eof do 
                    packets <- packets @ [ parsePacket sub ]
            { version = version; typeid = typeid; value = Op packets }
            
    let rec version packet =
        let subValue =
            match packet.value with
            | Op packets -> packets |> List.sumBy version 
            | _ -> 0L
        packet.version + subValue

    let rec calc packet =
        match packet.typeid, packet.value with
        | 4L, L v -> v |> int64
        | 0L, Op packets -> packets |> List.map calc |> List.sum 
        | 1L, Op packets -> packets |> List.map calc |> List.reduce (fun a b -> a * b)
        | 2L, Op packets -> packets |> List.map calc |> List.min
        | 3L, Op packets -> packets |> List.map calc |> List.max
        | 5L, Op packets -> let p = packets |> List.map calc in if p[0] > p[1] then 1L else 0L 
        | 6L, Op packets -> let p = packets |> List.map calc in if p[0] < p[1] then 1L else 0L 
        | 7L, Op packets -> let p = packets |> List.map calc in if p[0] = p[1] then 1L else 0L 
        | _ -> raise Unreachable

    let run text output =
        let stream = BitStream.parse text

        let p = parsePacket stream

        version p |> string |> output 1
        calc p |> string |> output 2

