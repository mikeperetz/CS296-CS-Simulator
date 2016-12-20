# CS 296-25 Honors Project

Mike Peretz
peretz2


INTRO:

Welcome! This is your apartment. You are just now starting your semester as a Computer Science major at UIUC! You have 30 days in total. Some things to note:

* At any point in time, you may (sleep), (study), (relax), or change locations. Given your heavy class schedule, you only have the chance to do one of these each day. 
* There are 3 distinct locations. Your apartment (home), the library (grainger), and the testing center (testcenter). You may navigate to these by typing home, grainger, or testcenter respectively. 
* On each of the 10th, 20th, and 30th days, you will have an exam. If you are not at the testing center by that date, you will miss your exam.
* You have 3 attributes to maintain: Insanity, Exhaustion, and Knowledge. Too much studying and not enough sleep and relaxation will take a large toll on your insanity and exhaustion levels. On the other hand, if you do not study enough, you will not do well on exams, even if you answer the questions correctly. It's a rough life. 
* It is your job to experiment with different command/room combinations. There is not much room for error. Poor planning will result in sure death.
* You may type (status) to view your current stats.
* Prepare to die a lot until you find your limits. Have fun!

DETAILS:

15 commands are:
look, help, quit, reset, sleep, study, relax, grainger, home, testcenter, status, a, b, c, d

I decided to tweak the multi-room adventure idea into a resource-management simulator, trading the concept of many objects and rooms for persistent attribute tracking and a "simulated" many-room experience generated by command/room combinations (e.g. studying at grainger will have a significantly different effect than studying at home, exams can only be taken in the testing center, etc.). There is also a small multiple-choice exam feature which stands in for the "puzzle" (though instead of obtaining an object, the reward/punishment is handled via resource allocation).


Run the game with "lein run". Controls are relatively self explanatory after that, and mechanics are meant to be obscure until observed.