module Book where

import Data.List (find)
import System.IO (hFlush, stdout)
import System.Directory (removeFile, renameFile)
import Text.Read (readMaybe)

data Book = Book { bookId :: Int
                 , bookTitle :: String
                 , bookAuthor :: String
                 , publishingYear :: Int
                 , price :: Float
                 , quantity :: Int
                 } deriving (Show, Eq)

addNewBook :: [Book] -> String -> String -> Int -> Float -> Int -> IO [Book]
addNewBook listBooks _bookTitle _bookAuthor _publishingYear _price _quantity = do
    let lastId = if null listBooks then 0 else bookId $ last listBooks
        newId = lastId + 1
        newBook = Book { bookId = newId
                       , bookTitle = _bookTitle
                       , bookAuthor = _bookAuthor
                       , publishingYear = _publishingYear
                       , price = _price
                       , quantity = _quantity
                       }
    let newListBooks = listBooks ++ [newBook]
    return newListBooks

editBook :: [Book] -> Int -> IO [Book]
editBook listBooks _bookId = do
    let bookToEdit = find (\book -> (bookId book) == _bookId) listBooks
    if bookToEdit == Nothing
        then do
            putStrLn "\nCannot find this book!"
            return listBooks
        else do
            _bookTitle <- getStringInput "\nInput new title: "
            _bookAuthor <- getStringInput "\nInput new author: "
            _publishingYear <- getIntInput "\nInput new publishing year: "
            _price <- getFloatInput "\nInput new price: "
            _quantity <- getIntInput "\nInput new quantity: "
            let newListBooks = replaceItem listBooks (unwrapItem bookToEdit) _bookTitle _bookAuthor _publishingYear _price _quantity
            putStrLn "\nEdited!"
            return newListBooks

unwrapItem :: Maybe Book -> Book
unwrapItem (Just a) = a

replaceItem :: [Book] -> Book -> String -> String -> Int -> Float -> Int -> [Book]
replaceItem [] bookToEdit _bookTitle _bookAuthor _publishingYear _price _quantity = []
replaceItem (book:otherBooks) bookToEdit _bookTitle _bookAuthor _publishingYear _price _quantity
    | book == bookToEdit = [book{
                            bookTitle       = case (not . null) _bookTitle of
                                                True -> _bookTitle 
                                                False -> (bookTitle book)

                           , bookAuthor     = case (not . null) _bookAuthor of
                                                True -> _bookAuthor
                                                False -> (bookAuthor book)

                           , publishingYear = case (< 0) _publishingYear of
                                                True -> (publishingYear book)
                                                False -> _publishingYear

                           , price          = case (< 0) _price of
                                                True -> (price book)
                                                False -> _price

                           , quantity       = case (< 0) _quantity of
                                                True -> (quantity book)
                                                False -> _quantity
                           }] ++ otherBooks
    | otherwise = [book] ++ replaceItem otherBooks bookToEdit _bookTitle _bookAuthor _publishingYear _price _quantity

deleteBook :: [Book] -> Int -> IO [Book]
deleteBook listBooks _bookId = do
    let bookToDelete = find (\book -> (bookId book) == _bookId) listBooks
    if bookToDelete == Nothing
        then do
            putStrLn "\nCannot find this book!"
            return listBooks
        else do
            let newListBooks = removeItem listBooks (unwrapItem bookToDelete)
            putStrLn "\nDeleted!"
            return newListBooks

removeItem :: [Book] -> Book -> [Book]
removeItem [] bookToDelete = []
removeItem (book:otherBooks) bookToDelete
    | book == bookToDelete = otherBooks
    | otherwise = [book] ++ removeItem otherBooks bookToDelete

writeListBooksToDB :: [Book] -> IO ()
writeListBooksToDB listBooks = do
    let str = convertListBooksToString listBooks
    writeFile "db/temp.txt" str
    removeFile "db/books.txt"
    renameFile "db/temp.txt" "db/books.txt"

convertListBooksToString :: [Book] -> String
convertListBooksToString [] = ""
convertListBooksToString (book:otherBooks) = show (bookId book) ++ " " ++
                                             bookTitle book ++ " " ++
                                             bookAuthor book ++ " " ++
                                             show (publishingYear book) ++ " " ++
                                             show (price book) ++ " " ++
                                             show (quantity book) ++ "\n" ++
                                             convertListBooksToString otherBooks

readListBooksFromDB :: String -> [Book]
readListBooksFromDB str = map readBook (lines str)

readBook :: String -> Book
readBook item = case words item of
    (_bookId:_bookTitle:_bookAuthor:_publishingYear:_price:_quantity:_) -> 
        Book { bookId = read _bookId
             , bookTitle = _bookTitle
             , bookAuthor = _bookAuthor
             , publishingYear = read _publishingYear
             , price = read _price
             , quantity = read _quantity
             }

showListBooks :: [Book] -> String
showListBooks [] = ""
showListBooks (book:otherBooks) = "\n" ++ replicate 20 '-' ++
                                  "\nBook ID: " ++ show (bookId book) ++
                                  "\nBook Title: " ++ show (bookTitle book) ++
                                  "\nBook Author: " ++ show (bookAuthor book) ++
                                  "\nPublishing Year: " ++ show (publishingYear book) ++
                                  "\nPrice: " ++ show (price book) ++
                                  "\nQuantity: " ++ show (quantity book) ++
                                  "\n" ++ replicate 20 '-' ++
                                  showListBooks otherBooks

getStringInput :: String -> IO String
getStringInput message = do
    putStr message
    hFlush stdout
    getLine

getIntInput :: String -> IO Int
getIntInput message = do
    putStr message
    hFlush stdout
    line <- getLine
    case readMaybe line of
        Just x -> return x
        Nothing -> return (-1)

getFloatInput :: String -> IO Float
getFloatInput message = do
    putStr message
    hFlush stdout
    line <- getLine
    case readMaybe line of
        Just x -> return x
        Nothing -> return (-1.0)
