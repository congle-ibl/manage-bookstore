module Book where

import Data.List
import System.IO
import System.Directory

data Book = Book { bookId :: Int
                 , bookTitle :: String
                 -- , bookAuthor :: String
                 -- , publishingYear :: Int
                 -- , price :: Float
                 -- , quantity :: Int
                 } | UnknownBook deriving (Show, Eq)

addNewBook :: [Book] -> String -> IO [Book]
-- addNewBook :: [Book] -> String -> String -> Int -> Float -> Int -> IO [Book]
addNewBook listBooks _bookTitle = do
-- addNewBook existingBooks _bookTitle _bookAuthor _publishingYear _price _quantity = do
    let lastId = if null listBooks then 0 else bookId $ last listBooks
        newId = lastId + 1
        newBook = Book { bookId = newId
                       , bookTitle = _bookTitle
                       -- , bookAuthor = _bookAuthor
                       -- , publishingYear = _publishingYear
                       -- , price = _price
                       -- , quantity = _quantity
                       }
    let newListBooks = listBooks ++ [newBook]
    return newListBooks

editBook :: [Book] -> Int -> String -> IO [Book]
editBook listBooks _bookId _bookTitle = do
    let bookToEdit = find (\book -> (bookId book) == _bookId) listBooks
    if (unwrapItem bookToEdit) == UnknownBook
        then do
            putStrLn "\nCannot find this book!"
            return listBooks
        else do
            let newListBooks = replaceItem listBooks (unwrapItem bookToEdit) _bookTitle
            putStrLn "\nEdited!"
            return newListBooks

unwrapItem :: Maybe Book -> Book
unwrapItem (Just a) = a
unwrapItem Nothing = UnknownBook

replaceItem :: [Book] -> Book -> String -> [Book]
replaceItem [] bookToEdit _bookTitle = []
replaceItem listBooks@(book:otherBooks) bookToEdit _bookTitle
    | null _bookTitle = listBooks
    | book == bookToEdit = [book{bookTitle = _bookTitle}] ++ otherBooks
    | otherwise = [book] ++ replaceItem otherBooks bookToEdit _bookTitle

deleteBook :: [Book] -> Int -> IO [Book]
deleteBook listBooks _bookId = do
    let bookToDelete = find (\book -> (bookId book) == _bookId) listBooks
    if (unwrapItem bookToDelete) == UnknownBook
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
    let str = init $ convertListBooksToString listBooks
    -- writeFile "db/books.txt" str
    writeFile "db/temp.txt" str
    removeFile "db/books.txt"
    renameFile "db/temp.txt" "db/books.txt"

convertListBooksToString :: [Book] -> String
convertListBooksToString [] = ""
convertListBooksToString (book:otherBooks) = show (bookId book) ++ " " ++
                                             bookTitle book ++ "\n" ++
                                             convertListBooksToString otherBooks

readListBooksFromDB :: String -> [Book]
readListBooksFromDB rawContent = map readBook (lines rawContent)

-- Change from case of to guard
readBook :: String -> Book
readBook item = case words item of
    (_bookId:_bookTitle:_) -> Book { bookId = read _bookId
                                   , bookTitle = _bookTitle
                                   }
    _ -> UnknownBook


showListBooks :: [Book] -> String
showListBooks [] = ""
showListBooks (book:otherBooks) = "\n" ++ replicate 20 '-' ++
                                  "\nBook ID: " ++ show (bookId book) ++
                                  "\nBook Title: " ++ show (bookTitle book) ++
                                  "\n" ++ replicate 20 '-' ++
                                  showListBooks otherBooks

getInput :: String -> IO String
getInput message = do
    putStr message
    hFlush stdout
    getLine
