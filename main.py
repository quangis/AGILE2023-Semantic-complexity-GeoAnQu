import json
import csv

from statistics import mean

#from geo_question_parser import QuestionParser
from geo_question_parser_haiqi.Identify_new import QuestionParser as QuestionParserNew


def parseConceptTree(corpusId = "GeoAnQu", corpusNS = "qac"):
    jsonArray = []

    # [SC] open corpus datafile
    with open(f"{corporaDir}\\{corpusId}.txt", 'r') as datacsvfile:
        datacsvreader = csv.DictReader(datacsvfile, delimiter=';')

        # parser = QuestionParser(None)
        parser = QuestionParserNew()

        # [SC] iterate through the questions in the corpus
        for q in datacsvreader:
            qId = q["ID"]
            qStr = q["Question"]

            if "Fixed" in q and q['Fixed'] == "0":
                continue

            qParsed = {}
            try:
                qParsed = parser.parseQuestion(qStr)
            except Exception as e:
                print("================================================== Exception")
                print(f"while parsing question with id {qId} and {qStr}")
                print(e)

            jsonObj = {
                "id": qId,
                "qParsed": qParsed
            }

            if "RQuestion" in q:
                rqStr = q["RQuestion"]
                rqParsed = {}
                try:
                    rqParsed = parser.parseQuestion(rqStr)
                except Exception as e:
                    print("================================================== Exception")
                    print(f"while parsing question with id {qId} and {rqStr}")
                    print(e)

                jsonObj["rqParsed"] = rqParsed

            jsonArray.append(jsonObj)

        with open(f"{dataOutputDir}\\{corpusId}.json", 'w') as f:
            json.dump(jsonArray, f, indent=4)

    return jsonArray


def generateStats(corpusId, revised=False):
    typesStr = ['conamount', 'amount', 'eventquality', 'objconobjconpro', 'field', 'placename', 'boolfield',
             'objconamount', 'network', 'distfield', 'aggre', 'object', 'objectquality', 'distanceBand', 'grid',
             'networkquality', 'allocation', 'proportion', 'event', 'eveconamount', 'covamount', 'location',
             'eveconobjconpro']
    typesStr.sort()

    # [SC] open the json file containing parsed results
    with open(f"{dataOutputDir}\\{corpusId}.json", 'r') as f\
            , open(f"{corporaDir}\\{corpusId}_missing.json", 'r') as fm:
        jsonArray = json.load(f)
        missingJsonArray = json.load(fm)
        jsonArray.extend(missingJsonArray)

        suffix = ""
        if revised:
            suffix = "_r"

        # [SC] type and transformation count summary is stored in this CSV file
        with open(f"{dataOutputDir}\\{corpusId}_ParserStats{suffix}.txt", 'w', newline='') as statsFile:
            fieldnames = ['ID', 'Question', 'qTypesCount', 'qTransCount', 'qOutputType']
            fieldnames.extend(typesStr)
            statsWriter = csv.DictWriter(statsFile, fieldnames=fieldnames, delimiter=";")
            statsWriter.writeheader()

            for qResult in jsonArray:
                newRow = {
                    "ID": qResult["id"],
                    "Question": "NA",
                    "qTypesCount": "NA",
                    "qTransCount": "NA",
                    'qOutputType': "NA"
                }
                for typeStr in typesStr:
                    newRow[typeStr] = "NA"

                nodeName = "qParsed"
                if revised:
                    nodeName = "rqParsed"

                # [SC] create a summary for the original question
                if nodeName in qResult and not qResult[nodeName]["question"] == "NA":
                    qParsed = qResult[nodeName]

                    newRow["Question"] = qParsed["question"]
                    newRow["qTypesCount"] = 0
                    newRow["qTransCount"] = 0

                    for typeStr in typesStr:
                        newRow[typeStr] = 0

                    if "cctrans" in qParsed:
                        if "types" in qParsed["cctrans"]:
                            newRow["qTypesCount"] = len(qParsed["cctrans"]["types"])

                            for typeObj in qParsed["cctrans"]["types"]:
                                typeStr = typeObj["type"]
                                newRow[typeStr] = newRow[typeStr] + 1

                        if "transformations" in qParsed["cctrans"]:
                            transCount = len(qParsed["cctrans"]["transformations"])
                            newRow["qTransCount"] = transCount

                            # [SC] extract type of the output
                            transesObj = qParsed["cctrans"]["transformations"]
                            for transIndex in range(transCount-1, -1, -1):
                                afterId = transesObj[transIndex]["after"][0]

                                finalOutput = True
                                for transObj in transesObj:
                                    if afterId in transObj["before"]:
                                        finalOutput = False

                                if finalOutput:
                                    for typeObj in qParsed["cctrans"]["types"]:
                                        if typeObj["id"] == afterId:
                                            newRow["qOutputType"] = typeObj["type"]
                                    break

                statsWriter.writerow(newRow)


def printBasicStats(corpusId):
    # [SC] open the json file containing parsed results
    with open(f"{dataOutputDir}\\{corpusId}.json", 'r') as f \
            , open(f"{corporaDir}\\{corpusId}_missing.json", 'r') as fm:
        jsonArray = json.load(f)
        missingJsonArray = json.load(fm)
        jsonArray.extend(missingJsonArray)

        qTypesCount = 0
        qTransCount = 0
        qTypeLengths = []
        qTransLengths = []

        rqTypesCount = 0
        rqTransCount = 0
        rqTypeLengths = []
        rqTransLengths = []

        for qResult in jsonArray:
            qParsed = qResult["qParsed"]
            if "cctrans" in qParsed:
                if "types" in qParsed["cctrans"] and qParsed["cctrans"]["types"]:
                    qTypesCount += 1
                    qTypeLengths.append(len(qParsed["cctrans"]["types"]))
                if "transformations" in qParsed["cctrans"] and qParsed["cctrans"]["transformations"]:
                    qTransCount += 1
                    qTransLengths.append(len(qParsed["cctrans"]["transformations"]))

            if "rqParsed" in qResult:
                rqParsed = qResult["rqParsed"]
                if "cctrans" in rqParsed:
                    if "types" in rqParsed["cctrans"] and rqParsed["cctrans"]["types"]:
                        rqTypesCount += 1
                        rqTypeLengths.append(len(rqParsed["cctrans"]["types"]))
                    if "transformations" in rqParsed["cctrans"] and rqParsed["cctrans"]["transformations"]:
                        rqTransCount += 1
                        rqTransLengths.append(len(rqParsed["cctrans"]["transformations"]))

        print(f"\nStats for the corpus {corpusId}:")
        print("\tOriginal questions => "\
            f"Types: {qTypesCount} ({round(qTypesCount*100/len(jsonArray))}%); "\
            f"Transformations: {qTransCount} ({round(qTransCount*100/len(jsonArray))}%); "\
            f"Types mean length: {round(mean(qTypeLengths), 1)}; "\
            f"Trans mean length: {round(mean(qTransLengths), 1)}")
        if len(rqTypeLengths) > 0:
            print("\tReformatted questions => "\
                f"Types: {rqTypesCount} ({round(rqTypesCount*100/len(jsonArray))}%); "\
                f"Transformations: {rqTransCount} ({round(rqTransCount*100/len(jsonArray))}%) "\
                f"Types mean length: {round(mean(rqTypeLengths), 1)}; "\
                f"Trans mean length: {round(mean(rqTransLengths), 1)}")


def compareToBaseline():
    baseline = "geo_question_parser_haiqi/orgResults/GeoAnQu_parser_results.json"
    output = "outputData/GeoAnQu.json"

    with open(output, 'r') as outf, open(baseline, 'r') as basef:
        outArray = json.load(outf)
        baseArray = json.load(basef)

        for index in range(len(baseArray)):
            # print(f"comparing {outArray[index]['qParsed']['question']}")

            if not baseArray[index]["question"] == outArray[index]["qParsed"]["question"]:
                print("========================== MISMATCHING questions")
                print(baseArray[index]["question"])
                print(outArray[index]["qParsed"]["question"])
            elif not json.dumps(baseArray[index]) == json.dumps(outArray[index]["qParsed"]):
                print("========================== MISMATCHING PARSE for questions")
                print(baseArray[index]["question"])
                print(outArray[index]["qParsed"]["question"])


def getUniqueTypes(corpusId="GeoAnQu"):
    with open(f"{dataOutputDir}\\{corpusId}.json", 'r') as f:
        jsonArray = json.load(f)

        uniqueTypes = set()

        for qResult in jsonArray:
            if ("qParsed" in qResult and
                    "cctrans" in qResult["qParsed"] and
                    "types" in qResult["qParsed"]["cctrans"]):

                for typeObj in qResult["qParsed"]["cctrans"]["types"]:
                    uniqueTypes.add(typeObj["type"])

            if ("rqParsed" in qResult and
                    "cctrans" in qResult["rqParsed"] and
                    "types" in qResult["rqParsed"]["cctrans"]):

                for typeObj in qResult["rqParsed"]["cctrans"]["types"]:
                    uniqueTypes.add(typeObj["type"])

        print(uniqueTypes)


if __name__ == "__main__":
    # [SC] folder with input corpora
    corporaDir = "inputCorpora"
    # [SC] data output folder
    dataOutputDir = "outputData"


    geoTwoJson = parseConceptTree("Geo201", "g201") # GeoQuestions201 corpus
    geoQueryJson = parseConceptTree("GeoQuery", "geoq") # GeoQuery corpus
    gikiJson = parseConceptTree("Giki", "giki") # GikiCLEF/GikiP corpora
    geoClefJson = parseConceptTree("GeoCLEF", "geoclef") # GeoCLEF corpora
    geoAnQuJson = parseConceptTree()  # GeoAnQu corpora

    generateStats("Geo201")
    generateStats("Geo201", True)
    generateStats("GeoQuery")
    generateStats("GeoQuery", True)
    generateStats("Giki")
    generateStats("Giki", True)
    generateStats("GeoCLEF")
    generateStats("GeoCLEF", True)
    generateStats("GeoAnQu")