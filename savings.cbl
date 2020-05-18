      *****************************************************************
      * SAVINGS                                                       *
      *                                                               *
      * A simple program that calculates the future value of an       *
      * initial investment in a savings account which is compounded   *
      * on a monthly basis.                                           *
      *                                                               *
      *****************************************************************
       identification division.
       program-id.   savings.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Savings account formula:                                      *
      *                                                               *
      *              F = P (1+r)^n                                    *
      *                                                               *
      *       F = Future Value                                        *
      *       P = Present Value (initial investment)                  *
      *       r = monthly-interest (rate)                             *
      *       n = term (months)                                       *
      *                                                               *
      *****************************************************************

       01 CALC-FIELDS.
              05 PRESENT-VALUE     PIC S9(9)V99 USAGE COMP.
              05 ANNUAL-INTEREST   PIC 99V9(4) USAGE COMP.
              05 MONTHLY-INTEREST  PIC 9V9(6) USAGE COMP.
              05 LOAN-TERM-YEARS   PIC 99 USAGE COMP.
              05 LOAN-TERM-MONTHS  PIC 999 USAGE COMP.
              05 NUMERATOR         PIC S9(9)V9(6) USAGE COMP.
              05 DENOMINATOR       PIC S9(9)V9(6) USAGE COMP.
              05 FUTURE-VALUE      PIC S9(9)V99 USAGE COMP.

       01 DISP-FIELDS.
              05 FV-OUT            PIC $ZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
              05 PRINCIPAL         PIC $ZZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
              05 INTEREST-RATE     PIC Z9.99 USAGE DISPLAY.
              05 LOAN-TERM-OUT     PIC Z9 USAGE DISPLAY.

       procedure division.
       init-ws.
              initialize calc-fields
              initialize disp-fields.

       user-input.
              display "SAVINGS ACCOUNT CALCULATOR"
              display "Enter zero for any parameter to end the program."
              display "Enter initial investment: "
              accept present-value
              if present-value = 0
                     go to end-program
              end-if
              if present-value > 999999999
                     display "Value must be <= $999,999,999.99"
                     go to user-input
              end-if

              display "Enter annual interest rate as a % value: "
              accept annual-interest
              if annual-interest = 0
                     go to end-program
              end-if
              if annual-interest > 26
                     display "Interest must be <= 26%"
                     go to user-input
              end-if

              display "Enter term in years: "
              accept loan-term-years
              if loan-term-years = 0
                     go to end-program
              end-if
              if loan-term-years > 30
                     display "Term must be <= 30 years."
                     go to user-input
              end-if.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Savings account formula:                                      *
      *                                                               *
      *              F = P (1+r)^n                                    *
      *                                                               *
      *       F = Future Value                                        *
      *       P = Present Value (initial investment)                  *
      *       r = monthly-interest (rate)                             *
      *       n = term (months)                                       *
      *                                                               *
      *****************************************************************

              move loan-term-years to loan-term-out
              move present-value to principal
              move annual-interest to interest-rate

              multiply 12 by loan-term-years giving loan-term-months
              divide annual-interest by 1200 giving monthly-interest
              rounded

              compute denominator = (1+monthly-interest) **
              loan-term-months

              compute future-value = present-value *
              denominator

              move future-value to fv-out.

       disp-result.
              display "Principal: " principal
              display "Term: " loan-term-out " years"
              display "Interest Rate: " interest-rate "%"
              display "Future Value: " fv-out.

       end-program.
              stop run.
