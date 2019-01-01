namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System.Text

// Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange).
// For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
// A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key.
// The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
// For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes.
// The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.
// Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
// If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
// The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
// Your task has been made easy, as the encryption key consists of three lower case characters.
// Using problem_059.dat, a data file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words,
// decrypt the message and find the sum of the ASCII values in the original text.

[<TestFixture>]
type Problem059() =

    let checkData = ["The "; " the "]

    let asciiEncoder = Encoding.ASCII
    let keyCharRangeStart = asciiEncoder.GetBytes("a").[0]
    let keyCharRangeEnd = asciiEncoder.GetBytes("z").[0]

    let repeatKey (size: int) (keyBasis: byte[]) =
        let key = Array.create size 0uy
        seq {0 .. size - 1} |> Seq.iter (fun index -> key.[index] <- keyBasis.[index % keyBasis.Length])
        key

    let decypt (source: byte[]) (key: byte[]) =
        seq {0 .. source.Length - 1} |> Seq.map (fun index -> source.[index] ^^^ key.[index]) |> Seq.toArray


    let solveImpl (dataFilename: string) =
        let source = File.ReadAllText(Path.Combine("Data", dataFilename)).Split(',') |> Seq.map (fun value -> value |> byte) |> Seq.toArray
        let result = seq {for key1 in keyCharRangeStart .. keyCharRangeEnd do
                          for key2 in keyCharRangeStart .. keyCharRangeEnd do
                          for key3 in keyCharRangeStart .. keyCharRangeEnd do yield [|key1; key2; key3|]} |>
                     Seq.map (fun keyBasis -> keyBasis |> repeatKey source.Length) |>
                     Seq.map (fun key -> key |> decypt source |> asciiEncoder.GetChars |> System.String) |>
                     Seq.filter (fun dest -> checkData |> List.exists dest.Contains) |> Seq.toList
        Assert.AreEqual(1, result.Length)
        result |> List.head |> asciiEncoder.GetBytes |> Seq.sumBy (fun value -> value |> int)

    [<TestCase("problem_059.dat", 107359, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)