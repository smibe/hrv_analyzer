

#r "nuget: FSharp.Stats" 
open System.IO
open System
open FSharp.Stats

fsi.AddPrinter<DateTime>(fun d -> d.ToShortDateString() + " " + d.ToLongTimeString()) 

let inputFile = __SOURCE_DIRECTORY__ + "/data/sample.fit"

let data = File.ReadAllBytes(inputFile)

type RecordType = Definition | Data

type Architecture = LittleEndian | BigEndian

type RecordHeader = { 
    RecordType: RecordType 
    TimeOffset: byte
    HasTimeOffset : bool
}

type DefinitionRecord = {
    Architecture : Architecture
    GlobalMessageNumber : uint16 
    FieldIds : int[]
}

type HrData = {
    mutable Timestamp: DateTime
    mutable Hr : int
}

type DataRecord = {
    GlobalMessageNumber : uint16
    Timestamp : DateTime
    Values : int[]
}

type GlobalMessageType = FileId = 0 | Record = 20

type FileIdFieldId = Type = 0 | Manufacturer = 1 | Product =2 | SerialNumber = 3| TimeCreated = 4 | Number = 5 | ProductName = 6
type RecordFieldId =  Timestamp = 253 | PositionLat = 0 | PositionLong = 1 | Altitude = 2 | HeartRate = 3 | Cadence = 4 | Distance = 5 | Speed = 6 | Power = 7 | Calories = 33
type Record = | Definition of DefinitionRecord | Data of DataRecord 

let readHrData (reader: BinaryReader) (definition: DefinitionRecord) =
    let mutable hrData =  { Timestamp = DateTime.Now; Hr = 0}
    for id in definition.FieldIds do
        let recordFieldId = enum<RecordFieldId>(id)
        match recordFieldId with
        | RecordFieldId.Timestamp -> 
            (hrData.Timestamp <- DateTimeOffset.FromUnixTimeSeconds((int64)(reader.ReadUInt32())).LocalDateTime) 
            |> ignore
        | RecordFieldId.PositionLat -> reader.ReadInt32() |> ignore
        | RecordFieldId.PositionLong -> reader.ReadInt32() |> ignore
        | RecordFieldId.Altitude -> reader.ReadUInt16() |> ignore
        | RecordFieldId.HeartRate -> (hrData.Hr <- (int)(reader.ReadByte())) |> ignore
        | RecordFieldId.Distance -> reader.ReadUInt32() |> ignore
        | RecordFieldId.Cadence -> reader.ReadByte() |> ignore
        | RecordFieldId.Speed -> reader.ReadUInt16() |> ignore
        | RecordFieldId.Power -> reader.ReadUInt16() |> ignore
        | RecordFieldId.Calories -> reader.ReadUInt16() |> ignore
        | _ -> printf "invalid record field %A \n" recordFieldId
    hrData

let readFileIdData (reader: BinaryReader) (definition: DefinitionRecord) =
    for id in definition.FieldIds do
        let fileIdFieldId = enum<FileIdFieldId>(id)
        match fileIdFieldId with
        | FileIdFieldId.Type -> reader.ReadByte() |> ignore
        | FileIdFieldId.Manufacturer -> reader.ReadUInt16() |> ignore
        | FileIdFieldId.Product -> reader.ReadUInt16() |> ignore
        | FileIdFieldId.SerialNumber -> reader.ReadUInt32() |> ignore
        | FileIdFieldId.TimeCreated -> reader.ReadUInt32() |> ignore
        | FileIdFieldId.Number -> reader.ReadUInt16() |> ignore
        | _ -> printf "invalid file id field %A\n" fileIdFieldId


let readHeader (reader : BinaryReader) = 
    let headerSize = reader.ReadByte()
    let protocolVersion = reader.ReadByte()
    let profileVersion = reader.ReadUInt16()
    let dataSize = reader.ReadUInt32()
    let signature = reader.ReadChars(4)
    let crc = reader.ReadUInt16()
    signature

let readRecorHeader (reader : BinaryReader) = 
    let header = reader.ReadByte()
    let recoredType = if (header &&& (byte) 0x40) = (byte) 0x40 then RecordType.Definition else RecordType.Data
    let hasTimeOffset = (header &&& (byte) 0x80) = (byte) 0x80
    let timeOffset = header &&& (byte) 0x0f
    { RecordType = recoredType; TimeOffset = timeOffset; HasTimeOffset = hasTimeOffset}


let readDefinitionRecord (reader : BinaryReader) = 
    let reserved = reader.ReadByte()
    let archByte = reader.ReadByte()
    let architecture = if archByte = 0uy then Architecture.LittleEndian else Architecture.BigEndian
    let globalMessageNumber = reader.ReadUInt16()
    let nofFields = (int) (reader.ReadByte())
    let mutable ids  = Array.create nofFields 0
    for x in 0 .. (nofFields-1) do 
        let fieldId = reader.ReadByte()
        printf "field Id %A\n" fieldId
        let size = reader.ReadByte()
        let baseType = reader.ReadByte()
        ids.[x] <- (int) fieldId
        
    { Architecture = architecture; GlobalMessageNumber = globalMessageNumber; FieldIds = ids } 


let toHrData (d:DataRecord) =
    { Timestamp = d.Timestamp; Hr = 1}

let skipAll (reader : BinaryReader) = 
    while (reader.PeekChar()) <> -1 do
        reader.ReadByte() |> ignore

let parseData (reader : BinaryReader)= 
    let mutable definition: DefinitionRecord = { Architecture = Architecture.LittleEndian; GlobalMessageNumber = 0us; FieldIds = [||] } 

    let header = readHeader reader
    seq {
        while (reader.PeekChar()) <> -1 do
            let messageType = readRecorHeader reader
            match messageType.RecordType with
            | RecordType.Data -> 
                match enum<GlobalMessageType>((int) definition.GlobalMessageNumber) with 
                | GlobalMessageType.FileId -> readFileIdData reader definition
                | GlobalMessageType.Record -> yield readHrData reader definition
                | _  -> 
                    printf "invalid message type %A\n" definition.GlobalMessageNumber 
                    skipAll reader
            | RecordType.Definition -> 
                definition <- readDefinitionRecord reader
    }

let parse = 
    let reader = new BinaryReader(File.Open(inputFile, FileMode.Open, FileAccess.Read, FileShare.Read), System.Text.Encoding.ASCII)
    let data = parseData reader |> Seq.toList
    reader.Close()
    data

let values = parse 

let sum = values |> List.fold (fun s x -> s + x.Hr) 0
let median = ((double) sum) / (double) values.Length

let hrs = values |> List.map (fun x -> (double) 60000.0 / (double) x.Hr)
mean hrs
stDev hrs
printf "HRV %f" (stDev hrs)
