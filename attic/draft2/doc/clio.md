# clio  reference
version 0.1

A reference for Alpaca's built-in Lisp

## Named literals

| Name | Description |  
| ---- | ----------- |  
| `nil` | false; the empty list |
| `true` | the canonical true value |

## Definitions

    (def var val) ; equivalent to CL's defparameter

    ;;; all functions are generic
    (def (fname arg1 arg2 ...)
      expr1
      expr2
      ...)

    (def (fname (arg1 type1) (arg2 type2) &optional &rest &key ...)
      expr1
      expr2
      ...)

    (defclass cname (super1 super2 ...)
      (slot1 :accessor slot1 :default nil :keyword :slot1)
      (slot2 ...)
      ...
      (:defaults :slot1 val1 :slot2 val2 ...))

    (defmacro (mname ...)
      ...)

## Special forms

    begin
    case
    cond
    ensure
    if
    let
    loop
    set!
    signal
    with-exit
    with-handlers
    with-open

## Basic types

    boolean
    number
    character
    symbol
    string
    list
    vector
    array
    map
    series
    stream
    function
    method
    class
    condition

## Text-editing classes

    buffer
    mark
    point
    text

## Windowing classes




