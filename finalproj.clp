/**
* Hilari Fan
* May 5, 2019
*
* This file is creates a workout and meal plan for the user given their conditions using backward chaining.
*
* ask              - gets the user input, reads the single word response, validates it or gives the user the chance to put a valid input, and returns the input
* isValid          - checks if the argument is a valid input depending on the output needed from the question 
*                    and tells the user if the input is invalid. The question inputs can be yes/no or integer, depending on the specifications
*                    since I have questions where the user responds with a positive integer, I also in each rule what the number refers to so that 
*                    everything is more understandable and they don't look like magic numbers
* start            - starting function that contains the beginning description for the user, resets and runs the program, and prints the workout and meal plan for the user
*/

(clear) 
(reset)

; backward chaining for the conditions/traits that are needed to create a workout

(do-backward-chaining bodyweight)
(do-backward-chaining fitnesslevel)
(do-backward-chaining injuries)
(do-backward-chaining goal)
(do-backward-chaining gender)
(do-backward-chaining puberty)
(do-backward-chaining period)



; this is the global variable for the output. Each rule adds the information about the workout onto this output

(defglobal ?*output* = "
The basic workout plan for everyone:
You should workout 4 days of the week and rest for the other three days. The resting period is necessary for your body to recover. Overtraining can lead or not taking rest days can lead to serious injuries. The best workout is training Monday and Tuesday, rest on Wednesday, training Thursday and Friday, and rest for the weekend. On each of the consecutive days, you will be alternating between upper body and lower body training. For example, do not train upper body on consecutive day since just like workout out everyday, you need to give your upper body a time to recover. Therefore your weekly schedule should look like this: upper, lower, rest, upper, lower, rest, rest or lower, upper, rest, lower, upper, rest, rest

For each workout, you need to do 3 to 5 sets of 8 to 12 repetitions for each of 6 to 8 exercise. Afterwards, you need to do 10 to 30 minutes cardio. 

Exercises fall into 8 categories: horizontal pull, horizontal push, vertical pull, vertical push, core, knee dominant, hip dominant, and accessory. The horizontal pull, horizontal push, vertical pull, and vertical push exercises are for upper body days while knee dominant and hip dominant exercises are for lower body days. Core and accessory exercises will be dispersed into each routine, but make sure that you do the accessory exercises in the end since they are simple movements and more isolated. 

On the upper body workout day, you should do 1 exercise from each of the categories listed above (1 vertical pull, 1 vertical push, 1 horizontal pull, and 1 horizontal push) plus 2 to 4 of core or accessory exercises.
On lower body days, you should do 2 knee dominants and 2 hip dominants plus 2 to 4 core or accessory exercises. The 2 knee dominants and 2 hip dominants should not be the same exercise and do not do the same type of movement consecutively (alternate knee, hip, knee, hip, cores/ accessory)
There are not fixed exercises you need to do. Follow the above instructions to pick the exercises for your routine. Listed below are all the exercises you can choose from based on category. 

Vertical pull: pull ups, chin-ups, lap pull downs or any exercise where your arms are straight up with a weight and pulled down. There are three types of grips that you can also choose from: neutral, wide, or close grip
Vertical push: military press, push press, strict press, double dumbbell overhead press, single dumbbell overhead press, incline bench press, and any other overhead press. 
Horizontal pull: dumbbell rows, Trx row, barbell row, cable row, banded row, or any other bar rows. These can be high or low row and bilateral or unilateral.
Horizontal push: bench press with barbell in any of the grips or dumbbell in bilateral, unilateral, or alternating or flys that can be flat, decline, or incline
Hip dominant: lunges in forward, lateral, or reverse; bridges in bilateral or unilateral, upper body or lower body elevated; or step-ups using dumbbell or barbell weights
Knee dominant: squats that are front loaded using barbell or goblet hold of a dumbbell or a kettlebell, back loaded with a barbell, split, or Bulgarian/ rear foot elevated
Core: planks, side planks, plank taps, or plank reaches all can add weight vest, dead bug or bird dog can add bands, bridge holds, Russian twists, pallof/ anti rotation presses using bands or cable machine, lifts/ chops using bands, cable machine, plates, dumbbell
Accessory: as stated above they are isolating exercises, such as bicep curls, tricep extensions, or corrective exercises

Don't forget to stretch before any workout :)


General meal plan advice for everyone:
No matter what your goal, body type, or food restrictions is, you will have a better meal plan if you increase your intake of \"good\" fats, fruits, and vegetables. You should also consume whole foods and avoid processed foods, sugar, alcohol, simple carbs, and \"bad\" fats. 
You should also consume foods with high protein and carbohydrates before and after your workout to make the most of your exercising. Remember to eat no sooner than 45 minutes before as fuel and also after to help your body recover. Don't forget to stay hydrated as well.
Examples of good proteins: chicken, eggs, tilapia, tuna, salmon, shrimp
Examples of good carbs: quinoa, brown rice, yams, oatmeal
Examples of good fats: peanut butter, olive oil, almonds, seeds

The specialized content for you:
")



/**
* overrides the ask function in utilities
* takes in an argument (the question to be asked to the user) and checks if the response is valid using the helper method
* continues to ask user for input until the response is valid and returns the response 
*/
(deffunction ask (?a) 
   (printout t ?a) 
   (bind ?answer (read))
   (bind ?validation (isValid ?a ?answer))

   (while (eq ?validation FALSE) then
      (bind ?newanswer (read))
      (bind ?validation (isValid ?a ?newanswer))
   )

   (return ?validation)
)

/**
* checks if the response is valid based on the question
* there are two types of question responses: yes/no and integer for the questions with multiple different choices
* returns the response of the user if the input is valid; otherwise returns FALSE
* takes in two arguments, the question and the user's input
*/
(deffunction isValid (?a ?answer)
   (if (not (eq (str-index "(Y/N)" ?a) FALSE)) then   ; since there are so few yes/no questions, in my rule I put (Y/N) in parenthesis 
                                                      ; so that the user remembers how to respond, which differentiates the type of question
      (if (and (symbolp ?answer) (or (= (sub-string 1 1 ?answer) y) (= (sub-string 1 1 ?answer) Y))) then  
         (bind ?response yes)
       elif (and (symbolp ?answer) (or (= (sub-string 1 1 ?answer) n) (= (sub-string 1 1 ?answer) N))) then 
         (bind ?response no)
       else
         (printout t "Input is invalid. Valid inputs include yes or no. ")
         (bind ?response FALSE)
      )
    else
      (for (bind ?i 7) (>= ?i 1) (-- ?i)   ; out of all of my questions, 7 is the greatest value for a valid input and 1 is the smallest
                                           ; trying to find the maximum valid input to check if the user's input is valid
         (bind ?numstring (sym-cat ?i))
         (if (not (eq (str-index ?numstring ?a) FALSE)) then
            (bind ?maxnum ?i)
            (bind ?i 0)
         )
      )
      (if (and (integerp ?answer) (and (>= ?answer 1) (<= ?answer ?maxnum))) then 
         (bind ?response ?answer)
       else
         (printout t "Input is invalid. Valid inputs include an integer within the bounds of the question. ")
         (bind ?response FALSE)
      )
   )
   (return ?response)
)



; start of meal plan rules

(defrule overweightmalemeal "rule for weight loss meal plan for males"
   (bodyweight yes) 
   (gender yes)
=>
   (bind ?*output* (str-cat ?*output* "- Since you need to lose weight, the amount you should eat is 25% carbohydrates, 45% fat, and 35% protein. You should eat 3 to 5 meals, where 2 are optional ones for if you get hungry. For the main meals, your portion control should be around 2 palms of protein dense foods, 2 fists of vegetables, 1 cupped handful of carb dense foods, and 3 thumbs of fat dense foods. 
"))
)

(defrule overweightfemalemeal "rule for weight loss meal plan for females"
   (bodyweight yes)
   (gender no)
=>
   (bind ?*output* (str-cat ?*output* "- Since you need to lose weight, the percentage you should eat is 25% carbohydrates, 45% fat, and 35% protein. You should eat 3 to 5 meals, where 2 are optional ones for if you get hungry. For the main meals, your portion control should be around 1 palm of protein dense foods, 1 fist of vegetables, 0.5 cupped handful of carb dense foods, and 2 thumbs of fat dense foods.
"))
)

(defrule generalmalemeal "rule for general fitness meal plan for males"
   (bodyweight no)
   (goal 1)   ; 1 is for general fitness
   (gender yes)
=>
   (bind ?*output* (str-cat ?*output* "-  Since you are aiming for general fitness, the percentage you should eat is 55% carbohydrates, 25% protein, and 20% fat. You should eat 3 to 5 meals, where 2 are optional ones for if you get hungry. For the main meals, your portion control should be around 2 palms of protein dense foods, 2 fists of vegetables, 3 cupped handfuls of carb dense foods, 1 thumb of fat dense foods.
"))
)

(defrule generalfemalemeal "rule for general fitness meal plan for females"
   (bodyweight no)
   (goal 1)   ; 1 is for general fitness
   (gender no)
=>
   (bind ?*output* (str-cat ?*output* "-  Since you are aiming for general fitness, the percentage you should eat is 55% carbohydrates, 25% protein, and 20% fat. You should eat 3 to 5 meals, where 2 are optional ones for if you get hungry. For the main meals, your portion control should be around 1 palm of protein dense foods, 1 fist of vegetables, 2 cupped handfuls of carb dense foods, 0.5 thumb of fat dense foods.
"))
)

(defrule buildmusclemalemeal "rule for building muscle meal plan for males"
   (goal 2)   ; 2 is for building muscle
   (gender yes)
=>
   (bind ?*output* (str-cat ?*output* "- Since you are trying to bulk up, you should be eating more calories, around 3,000 per day. The percentage you should eat is 40% carbohydrate, 30% protein, and 30% fat can work well. You should eat 3 to 5 meals, where 2 are optional ones for if you get hungry. For the main meals, your portion control should be around 2 palms of protein dense foods, 2 fists of vegetables, 2 cupped handfuls of carb dense foods, 2 thumb of fat dense foods.
"))
)

(defrule buildmusclefemalemeal "rule for building muscle meal plan for females"
   (goal 2)   ; 2 is for building muscle
   (gender no)
=>
   (bind ?*output* (str-cat ?*output* "- Since you are trying to bulk up, you should be eating more calories, around 3,000 per day. The percentage you should eat is 40% carbohydrate, 30% protein, and 30% fat can work well. You should eat 3 to 5 meals, where 2 are optional ones for if you get hungry. For the main meals, your portion control should be around 1 palm of protein dense foods, 1 fist of vegetables, 1 cupped handfuls of carb dense foods, 1 thumb of fat dense foods.
"))
)



; start of workout plan rules

(defrule cai "rule for chronic ankle instability"
   (injuries 1)   ; 1 is for chronic ankle instability
=>
   (bind ?*output* (str-cat ?*output* "- Since you have chronic ankle instability, you need to change the stance of some of your movements. The vertical and horizontal pull or push will not change since these pertain to the upper body. For hip dominant, knee dominant, and core, you must change your stance to half kneeling, tall kneeling, quadruped, or prone since these are the stances that are not on your feet. Depending on what your doctor says, you will be able to choose between half and tall kneeling. If you cannot put any pressure on your ankles, do tall kneeling. However, if you have a little more range of motion, then do half kneeling to put a little bit of weight on your ankle. This idea of slowly increasing the weight on your injury is called progressive overloading. 
"))
)

(defrule shoulder "rule for shoulder impingement or rotator cuff injuries"
   (or (injuries 3) (injuries 2))   ; 2 is for shoulder impingement and 3 is for rotator cuff injuries
                                    ; both are shoulder injuries so the change in movement is the same
=>
   (bind ?*output* (str-cat ?*output* "- Since you have a shoulder injury, there is mostly movement changes. The lower body exercises, hip and knee dominant, will not change. However, your upper body movements, vertical / horizontal pull and push, will change to become accessory movements. This change is to work on stability and isolate your injury. For core movements, you should avoid quadruped exercises unless your doctor allows them.   
"))
)

(defrule lbp "rule for low back pain"
   (injuries 4)   ; 4 is for low back pain
=>
   (bind ?*output* (str-cat ?*output* "- Since you have low back pain, you need to change your stances. The only stances you should do are the ones where you are on the floor: prone, supine, or quadruped. You should then progressively overload until you are standing. You should also focus on the core and hip dominant exercises more.  
"))
)


(defrule ss "rule for shin splints"
   (injuries 5)   ; 5 is for shin splints
=>
   (bind ?*output* (str-cat ?*output* "- Since you have shin splints, you need to mainly pay attention to load management, particularly regarding knee and hip dominant exercises. Do not use heavy weights for your lower body exercises. All of the upper body exercises (vertical / horizontal pull or push) will stay the same. 
"))
)

(defrule hi "rule for hip impingement"
   (injuries 6)   ; 6 is for hip impingement
=>
   (bind ?*output* (str-cat ?*output* "- Since you have hip impingement, you will not have stance changes. However, you need to consult a doctor for your range of motion. You should pay attention to where you take the load to in order to have more pain free range of motion. This means that, for example, rather than squatting all the way to 90 degrees, squat to a larger angle / do not go too low.  
"))
)


(defrule prepubescent 
   (puberty 1)   ; 1 is for before puberty
=>
   (bind ?*output* (str-cat ?*output* "- Since you have not started puberty, your bones are not fully developed so you are more prone to injuries. Thus, do not have a high intensity load (large weight) or do not use weights at all.
"))
)

(defrule injurycardio "cardio for people with injuries"
   (not (injuries 7))   ; 7 is for no injuries, so this is for all injuries
=>
   (bind ?*output* (str-cat ?*output* "- Since you have an injury, you cannot do any running activity as running uses all parts of the body. Thus, the best cardio for you is swimming or any water aerobic exercise since it is low impact. You should swim for about 30 minutes at your own pace. Regardless of the stroke, this method is extremely effective. However, you should consult your doctor about the range of motion you have since swimming is still a full body exercise.
"))
)

(defrule musclecardio "cardio for someone who has a goal of gaining muscle"
   (goal 2)     ; 2 is for gaining muscle
   (injuries 7) ; 7 is for no injuries
=>
   (bind ?*output* (str-cat ?*output* "- Since your main objective is the gain muscle, make sure that you keep your cardio very moderate and minimal as intense cardio can cause your muscles to break down, which is the opposite of what you want. Thus, you can do a run for less than 30 minutes, but the best cardio for you is sprinting. Just like your weights, start small with your sprints, such as 5 10-second sprints with 1 to 5 minute rest in between each time. Then you can increase the time or the yards to challenge yourself even more in the following weeks. 
"))
)

(defrule cardio "cardio for general fitness"
   (bodyweight no)
   (goal 1)     ; 1 is for general fitness
   (injuries 7) ; 7 is for no injuries
=>
   (bind ?*output* (str-cat ?*output* "- There are two types of cardio: anaerobic or aerobic. Both are effective in their own way. Thus, you can choose whichever one you prefer. Anaerobic exercises involves sprints or any burst of activity in a short period of time. Aerobic exercises involve jogs or any light activity spread across a longer period of time. If you like endurance exercises then you can do a 30 minute jog. Otherwise, you can do sprinting. Sprinting is up to your own interpretation. You can keep the time or yards constant. In general, you can do 3 to 5 10-30 second sprints with around 2 to 5 minutes of rest in between each time. It is better if you start small (10 seconds) and increase the seconds or yards in the following weeks. 
"))
)

(defrule overweightcardio "general cardio for overweight person"
   (bodyweight yes)
   (injuries 7)   ; 7 is for no injuries
=>
   (bind ?*output* (str-cat ?*output* "- For your body type, the best cardio for you is walking. Since walking is low impact and a no weight exercise, it is the most suitable for someone who is overweight or obese. Starting with difficult activities, such as sprinting or HIIT can be damaging to your joints due to extra weight. Thus, you should start by walking for 30 minutes, and while the weight is coming off or you feel more comfortable, you can increase your speed into a light jog.     
"))
)

(defrule female "rule for females"
   (gender no)
   (or (injuries 7) (injuries 2) (injuries 3))   ; 7 is no injuries, 2 is shoulder impingement, 3 is rotator cuff injuries
                                                 ; need this condition since with some injuries, jumping exercises are not advised, so comment will not apply
=>
   (bind ?*output* (str-cat ?*output* "- Females tend to buckle their knees when they jump due to commonly wider hips than males. Thus, be more careful of your stance and movement when doing jumping exercises.
"))
)

(defrule femaleperiod "rule for females on their period"
   (gender no)
   (period yes)
=>
   (bind ?*output* (str-cat ?*output* "- Since you have started you period, make sure that you are extra careful during that time. During a period, you are more susceptible to injuries, so please make some changes to the routine. You should decrease your weights, pay more attention to your abilities, and slow down if you feel nauseous or lightheaded. Also remember to do the workouts to the best of your ability at all times.
"))
)

(defrule hiit "rule for hiit workout"
   (fitnesslevel 3)   ; 3 is for advanced athlete
   (injuries 7)       ; 7 is for no injuries
   (bodyweight no)
   (goal 1)           ; 1 is for general fitness
=>
   (bind ?*output* (str-cat ?*output* "- Since you are an advanced athlete, the cardio you can be doing is called HIIT. HIIT stands for High Intensity Interval Training. It is essentially a training technique where you put in 100 % effort through quick and intense bursts of exercise, which are followed by short recovery periods. This workout keeps your heart rate up and is great for burning fat in a short amount of time. HIIT workouts are usually a 1:1 work:rest ratio where the exercise is usually for 30 seconds to 1 minute depending on your athletic ability. This means that you, for example, do 30 seconds of burpees then 30 seconds of rest until you do the next exercise. There are usually 6 to 8 exercises in a routine, which should be 10 to 20 minutes. Exercises include but are not limited to: butt kicks, jump squats, burpees, mountain climbers, side lunges, jumping lunge, plank, plank jacks, high-knees, inchworm, fast feet shuffle, and skaters. Basically, bodyweight exercise that involve any jumping, running, or high intensity movement. If you do not feel comfortable with this difficulty, then do the other workout explained for you.
"))
)

(defrule femalehiit "rule for hiit workouts for females on their period"
   (fitnesslevel 3)   ; 3 is for advanced athlete 
   (injuries 7)       ; 7 is for no injuries
   (bodyweight no)
   (goal 1)           ; 1 is for general fitness
   (gender no)
   (period yes)
=>
   (bind ?*output* (str-cat ?*output* "- When you are on your period, do not do the designated HIIT workout. Instead, do a 10 to 30 minute jog.
"))
)



; start of the backward chained trait rules

(defrule need-gender "rule for your gender"
   (need-gender ?)
=>
   (bind ?x (ask "Are you a male? (Y/N) "))
   (assert (gender ?x))
)

(defrule need-fitnesslevel "rule for fitness level"
   (need-fitnesslevel ?)
=>
   (bind ?x (ask "What is your fitness level? Press 1 for beginner, 2 for intermediate, and 3 for advanced "))
   (assert (fitnesslevel ?x))
)

(defrule need-injuries "rule for any injuries"
   (need-injuries ?)
=>
   (bind ?x (ask "Do you have any of these injuries? If yes, press 1 for chronic ankle instability, 2 for shoulder impingement, 3 for rotator cuff injuries, 4 for low back pain, 5 for shin splints, 6 for hip impingement, and 7 for no injuries. If you have any of these injuries, you need to consult your doctor to find your range of motion and ability to use that part of your body. More than one of these injuries will not be accepted in this program since the workout would need to be extremely specialized for each individual "))
   (assert (injuries ?x))
)

(defrule need-period "rule for whether a female has started her period"
   (need-period ?)
   (or (puberty 2) (puberty 3))   ; a female cannot have started her period if she has not started puberty
   (gender no)                    ; if you are a male you do not have a period
=>
   (bind ?x (ask "Have you started your period? (Y/N) "))
   (assert (period ?x))
)

(defrule need-puberty
   (need-puberty ?)
=>
   (bind ?x (ask "Where are you in puberty? Press 1 for prepubescent, 2 for in puberty, 3 for post puberty "))
   (assert (puberty ?x))
)

(defrule need-goal
   (need-goal ?)
   (bodyweight no)   ; for someone who is overweight, their #1 goal has to be weight loss so they will not have the chance to choose their goal
=>
   (bind ?x (ask "What is your goal? Press 1 for general fitness/weight loss, 2 for strength/ gaining muscle "))
   (assert (goal ?x))
)

(defrule need-bodyweight
   (need-bodyweight ?)
=>
   (bind ?x (ask "Are you overweight/obese? (Y/N) "))
   (assert (bodyweight ?x))
)

/**
* starting function with the intro description of the system and also resets and runs
*/
(deffunction start ()
   (printout t "Hello! This is an expert system designed to give you a workout and meal plan, considering your gender, weight/ body type, goal, and more. There are a few things you should consider first.
1. There is no time question. The workout should take around 45 to 1 hour to do. If you have less free time to exercise, then you need to make time for your workout since you cannot have a sustainable workout routine in under 30 minutes. You must invest time into your workout routine in order to get the results that you want :)
2. Most of the exercises except for the cardio will stay the same for everyone. The big difference is how you do the exercise and the weight. This will all be specified, so make sure that you pay close attention to the specifications as those are very important to make sure that you are staying safe and happy :)
3. If you do not know how to do the exercise, make sure you consult a trainer or a credible source on the internet since form is also crucial to avoid injuries :) 
Please answer with a response starting with y or n (not case sensitive) for yes or no if the question does not specify how to respond
" crlf)

(reset)
(run)
(printout t ?*output*)
)


