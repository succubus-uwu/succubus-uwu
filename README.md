## Hi, I'm Anton Kochergin!

<p><em>· System Software at <a href="https://itmo.ru/">ITMO University</a> 🎓</em></p>
<p><em>· Backend Developer at <a href="https://extremum.info/">Extremum</a> 💼</em></p>

<br>

<img src="https://raw.githubusercontent.com/innng/innng/master/assets/kyubey.gif" height="24"  alt=""/> A little about me..

I am interested in developing compilers, but I am also interested in backend development (web) for commercial purposes.
- Web Development Stack: Java Spring, Relational and Non-Relational Databases, Object Storage (S3). I use the most suitable tools for each task, and if there are no suitable tools, I modify others (such as the [gateway api](https://github.com/succubus-uwu/Custom-Spring-Gateway)).
- Compiler development stack: I can implement various parsers (from recursive descent to GLR), and I know how to avoid recursion in the algorithms of the compiler's syntax, semantic, and subsequent frontend stages. I have limited knowledge of optimization and backends (but I am good at reading documentation).
- I have knowledge in Computer Science and some knowledge in Machine Learning.

_In fact, I can write in anything if you already have a code base. I've mainly worked with Java, Kotlin, and C._

## 2025

I moved to St. Petersburg to enroll in the first year of the Bachelor's 
[System Software](https://abit.itmo.ru/program/bachelor/system_software) 
program at ITMO University.

___The results have not yet been announced, but this year has been the most productive___

## 2024

Completed training at ProfSoft in UX/UI Design. [(cert)](certificate/UX%3AUI%20Design.pdf)

The website, which was implemented in the context of Extremum, easily handled thousands of users during the period of active preparation for exams.

The compiler has been rewritten, a custom parser has been implemented (without recursion), and the type system and functional programming have been started.

## 2023

I implemented my first compiler, which included semantic analysis but had a modest type system and a buggy parser. Fortunately, the source code was not saved.

I started working at Extremum, an online school for preparing for the Unified State Exam, as a backend developer (combining other specialties due to a staff shortage).

I won a data analysis hackathon solo. The track was voice gender recognition. I received the opportunity to study at ProfSoft in any field without entrance exams. (Saratov State University).

## 2020-2022

A modest start to a self-taught programmer's career, starting at the age of 12-13.

### My last Haskell program (for university)

Hamming code (7;4)

```hs
module Main (main) where

import Data.Char (digitToInt)
import Data.Bits (xor)
import Control.Monad (when)

bit :: [Int] -> Int -> Int
bit xs i = xs !! (i - 1)

syndrome :: [Int] -> Int
syndrome xs =
    let p1 = bit xs 1 `xor` bit xs 3 `xor` bit xs 5 `xor` bit xs 7
        p2 = bit xs 2 `xor` bit xs 3 `xor` bit xs 6 `xor` bit xs 7
        p4 = bit xs 4 `xor` bit xs 5 `xor` bit xs 6 `xor` bit xs 7
    in  p1 * 1 + p2 * 2 + p4 * 4

fixError :: [Int] -> Int -> [Int]
fixError xs 0 = xs
fixError xs pos =
    [ if i == pos then 1 - b else b | (b, i) <- zip xs [1..] ]

extractDataBits :: [Int] -> [Int]
extractDataBits xs = map (bit xs) [3, 5, 6, 7]

decodeHamming :: [Int] -> IO ()
decodeHamming bits = do
    let s = syndrome bits
    putStrLn $ "Syndrome: " ++ show s
    let corrected = fixError bits s
    when (s /= 0) $
        putStrLn $ "Error at bit " ++ show s ++ " (fixed)."
    let dataBits = extractDataBits corrected
    putStrLn $ "Data bits: " ++ concatMap show dataBits

main :: IO ()
main = do
    putStrLn "Enter a 7-bit message (e.g. 1011011):"
    input <- getLine
    let bits = map digitToInt input
    if length bits /= 7
        then putStrLn "Error: please enter exactly 7 bits."
        else decodeHamming bits
```
