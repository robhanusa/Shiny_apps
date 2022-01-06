# Shiny_apps

This repository contains a few shiny apps that are inspired by situations from past engineering roles and other moments in life. 

## Control systems 
This is a simple model of a PID controller’s response to a unit step. The delay and bias can be changed to model different systems, and the controllers proportional (Kp), Integral (1/Ki), and derivative (Kd) parameters can be tuned to get a stable response.

## Attribute sample sizing
This helps predict the number of samples from a population that need to be tested for an attribute (pass/fail) in order to achieve a desired level of confidence and reliability. This app currently allows you to predict the sample requirements in the case that up to 3 failures are detected. This can be a useful tool for planning validation activities or adjusting sampling requirements as the result of an observed failure.

## MTTF
Also "Mean Time To Failure," is inspired by reliability testing, in which several samples undergo repeated use cycles until they reach a certain cycle count or fail. This model uses a mean time to failure calculation to predict the performance of a product based on reliability testing on up to 8 samples. The total cycles each sample underwent is input, as well as whether the sample failed or not. The graph shows the predicted MTTF at a range of confidence levels, with an indicator at 95%.

## House repayment calculator
This came in handy when we were looking at buying a house. Some banks have something similar, but here we can see the total amount we will have paid at each point in time if we pay off the house early, or wait until the end, accounting both for total interest paid and early payment fee (common in Belgium). Just for fun I added the option to compare the amount paid to the market return if we were to have put the down payment in the market instead (interpret this as if you were to buy the house in either case, but you're wondering if you're better off contributing more to the down payment or investing in the market). Monthly payment is also calculated and compared to our budget in a bar graph.

## Material Resource Planning
This helps estimate the stock of two raw materials (one per tab) based on up to 3 orders, and up to 3 activities (products) that consume the raw materials. The lead time for the deliveries is also included to help the user determine when the orders for raw materials need to be placed. Raw material consumption per product/activity is hardcoded, so this needs to be manually adjusted. In a future version I might allow consumption to be more easily imported.