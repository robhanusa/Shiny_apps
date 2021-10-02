# Shiny_apps

This repository contains a few shiny apps that are inspired by situations from past engineering roles. 

**Control systems** is a simple model of a PID controller’s response to a unit step. The delay and bias can be changed to model different systems, and the controllers proportional (Kp), Integral (1/Ki), and derivative (Kd) parameters can be tuned to get a stable response.

**Attribute sample sizing** helps predict the number of samples from a population that need to be tested for an attribute (pass/fail) in order to achieve a desired level of confidence and reliability. This app currently allows you to predict the sample requirements in the case that up to 3 failures are detected. This can be a useful tool for planning validation activities or adjusting sampling requirements as the result of an observed failure.

**MTTF**, or Mean Time To Failure, is inspired by reliability testing, in which several samples undergo repeated use cycles until they reach a certain cycle count or fail. This model uses a mean time to failure calculation to predict the performance of a product based on reliability testing on up to 8 samples. The total cycles each sample underwent is input, as well as whether the sample failed or not. The graph shows the predicted MTTF at a range of confidence levels, with an indicator at 95%.
