//
//  main.swift
//  TestCMDLINE
//
//  Created by Jacob Mather on 6/6/14.
//  Copyright (c) 2014 Jacob Mather. All rights reserved.
//
// /Applications/Xcode6-Beta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/swift -sdk /Applications/Xcode6-Beta.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk


// Stolen ruthlessly from
// http://stackoverflow.com/questions/24034043/how-do-i-check-if-a-string-contains-another-string-in-swift
extension String {
    func contains(other: String) -> Bool{
        let len = countElements(self)
        
        for var i = 0; i < len; i++ {
            var subString = self[Range(start: i, end: i + 1)]

            if subString.hasPrefix(other){
                return true
            }
        }
        
        return false
    }
}

// Stolen ruthlessly from
// http://stackoverflow.com/questions/24044851/how-do-you-use-string-substringwithrange-or-how-do-ranges-work-in-swift
extension String {
    subscript (r: Range<Int>) -> String {
        get {
            let startIndex = advance(self.startIndex, r.startIndex)
            let endIndex = advance(startIndex, r.endIndex - r.startIndex)
            
            return self[Range(start: startIndex, end: endIndex)]
        }
    }
}

import Foundation

let SOLUTION_LENGTH = 4
let MAX_GUESSES = 10
let GUESS_WRONG = "_"
let GUESS_RIGHT = "X"
let GUESS_PRESENT = "*"
let CHOICES = ["A", "B", "C", "D", "E", "F"]

// Library code
func gameIntro() {
    println("Let's play Mastermind! The rules are easy, I promise.")
    println("I will pick \(SOLUTION_LENGTH) letters out of a possible \(CHOICES.count).")
    println("You will then get \(MAX_GUESSES) chances to guess which letters I picked.")
    println("For each letter you guess, I will answer as follows:")
    println("\t\(GUESS_WRONG) means you got it completely wrong.")
    println("\t\(GUESS_PRESENT) means you guessed a letter I used, but it's in the wrong position.")
    println("\t\(GUESS_RIGHT) means you guessed that letter right!")
    println("")
}

func generateSolution() -> String {
    var solution = "";
    
    while (countElements(solution) < SOLUTION_LENGTH) {
        let choice = Int(arc4random_uniform(UInt32(countElements(CHOICES))))
        let pick = CHOICES[choice]
        if solution.contains(pick) {
            continue
        }
        
        solution += pick
    }
    
    return solution
}

func isValidGuess(guess: String) -> Bool {
    let combinedChoices = "".join(CHOICES)
    let len = countElements(guess)

    if len != SOLUTION_LENGTH {
        return false
    }
    
    for var i = 0; i < len; i++ {
        var subString = guess[Range(start: i, end: i + 1)]
        if combinedChoices.contains(subString) == false {
            return false
        }
    }
    
    return true
}

func askGuess() -> String {
    println("Available pegs: " + ", ".join(CHOICES))
    print("Enter your guess (pick \(SOLUTION_LENGTH)): ")
    let standardInput = NSFileHandle.fileHandleWithStandardInput()
    let data = standardInput.availableData
    return String(NSString(data: data, encoding: NSUTF8StringEncoding))[Range(start: 0, end: 4)].uppercaseString;
}

func getGuess() -> String {
    var guess = askGuess()
    
    while isValidGuess(guess) == false {
        println("We were unable to understand your input. Please enter only the letters of your guess and press enter.")
        guess = askGuess()
    }
    
    return guess
}

func analyzeGuess(#solution: String, #guess: String) -> String {
    var results = String[]()
    
    for var i = 0; i < SOLUTION_LENGTH; i++ {
        let solution_char = solution[Range(start: i, end: i+1)]
        let guess_char = guess[Range(start: i, end: i+1)]
        
        if solution_char == guess_char {
            results.append(GUESS_RIGHT)
        } else if solution.contains(guess_char) {
            results.append(GUESS_PRESENT)
        } else {
            results.append(GUESS_WRONG)
        }
    }
    
    return "".join(results)
}

var solution = generateSolution()
var guesses = 0;

gameIntro()

while true {
    if guesses > MAX_GUESSES {
        println("You've ran out of guesses! Better luck next time!")
        break
    }
    
    let guess = getGuess()
    
    if guess == solution {
        println("You've guessed correctly, you win!")
        break
    }
    
    let analysis = analyzeGuess(solution: solution, guess: guess)
    println("Result: \(analysis)")
    
    guesses++
}