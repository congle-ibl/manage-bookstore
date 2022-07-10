# Manage Bookstore

## How to run:

1. Open terminal and change your directory to this project.

2. Build project: `cabal build`.

3. Run project: `cabal run`.

## Features:

This application is to manage a bookstore with basic actions:
+ Add a new book to the bookstore, a book has 6 attributes: id, title, author, publishing year, price and quantity.
+ Show all books in the bookstore.
+ Edit a book by id.
+ Delete a book by id.

The application's menu:
```
MENU:
--------------------
1. Add a new book
2. Edit a book
3. Delete a book
4. Show all books
5. Exit
--------------------

What do you want to do?:
```

1. Add a new book:
The book id has been increased automatically when adding a new book. You need to fill some information as below.
```
MENU:
--------------------
1. Add a new book
2. Edit a book
3. Delete a book
4. Show all books
5. Exit
--------------------

What do you want to do?: 1

Title: book1

Author: author1

Publishing Year: 2022

Price: 10

Quantity: 100
```

2. Show all books:
This feature is to show all the books in the bookstore.
```
MENU:
--------------------
1. Add a new book
2. Edit a book
3. Delete a book
4. Show all books
5. Exit
--------------------

What do you want to do?: 4

ALL BOOKS:

--------------------
Book ID: 1
Book Title: "book1"
Book Author: "author1"
Publishing Year: 2022
Price: 10.0
Quantity: 100
--------------------
```

3. Edit a book:
A book can be edited by an id. If it doesn't exist, the application will throw the message: "Cannot find this book!".
```
MENU:
--------------------
1. Add a new book
2. Edit a book
3. Delete a book
4. Show all books
5. Exit
--------------------

What do you want to do?: 2

Input your book id to EDIT: 2

Cannot find this book!
```

If this book exists in the bookstore, please enter the new title/author/publishing year/price/quantity to edit. If you ignore a field, it will keep the current value of this field.
```
MENU:
--------------------
1. Add a new book
2. Edit a book
3. Delete a book
4. Show all books
5. Exit
--------------------

What do you want to do?: 2

Input your book id to EDIT: 1

Input new title: Learn You A Haskell      

Input new author: 

Input new publishing year: 2020

Input new price: 20

Input new quantity: 

Edited!
```

The result after editing:
```
ALL BOOKS:

--------------------
Book ID: 1
Book Title: "Learn You A Haskell"
Book Author: "author1"
Publishing Year: 2020
Price: 20.0
Quantity: 100
--------------------
```

4. Delete a book:
A book can be deleted by an id. If it doesn't exist, the application will throw the message: "Cannot find this book!".
```
MENU:
--------------------
1. Add a new book
2. Edit a book
3. Delete a book
4. Show all books
5. Exit
--------------------

What do you want to do?: 3

Input your book id to DELETE: 2

Cannot find this book!

```

If this book exists in the bookstore, it will be deleted.
