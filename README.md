# Command Line Based To Do List Application Using Haskell and Brick

## Group Members
Jiajie Wang, Zihan Lyu, Zheming Pu

## How to install 
1. `git clone https://github.com/zhemp/Haskell-ToDo.git`
2. `cd Haskell-ToDo`
3. `stack install`
4. `stack run`

## Overview
Most people have a need to keep track of their everyday tasks in an organized manner. As graduate students and command line aficionados, we're all about efficiency and simplicity. That's why we're taking on the challenge of developing a command line based To-Do List application using Haskell, with a special focus on the Brick library for that slick user interface. Our aim of this project is to deliver a user-friendly interface and some cool features. The following features and timeline are projected and expected. 
## Progress at Dec 15 
### Key components 
The project can be divided into 3 sections at the first glance from a vertical view.  
<img width="1086" alt="Screenshot 2023-12-15 at 1 25 45 AM" src="https://github.com/zhemp/Haskell-ToDo/assets/83204691/2d7d5dac-8005-4cad-ba28-2270163f7b8e">


1. The first section is a universal status bar. This bar could remind the user of the number of tasks he/she has done and to do, as well as the current theme. But the most important effect is that it make the app looks better by leave margins between middle section and the ceiling.
2. The second section can be divided into two parts. The right part is a list of done tasks, which is developed to collect the task that user have done, We believe this could provide user with a sense of achievement. The left part contains four lists of task of different priority level. From top to bottom, they are "important and uergent", "urgent", "important", "not important and not urgent". We believe this could enable user to schedule their time in a more reasonable and clear manner. User can create, delete, modify main tasks and subtasks. Also, user could tag certain tasks as done. If a main task is tagged as done, it would be moved to the done list. User can also restore it if they operate by mistake.
![image](https://github.com/zhemp/Haskell-ToDo/assets/83204691/d5be1571-a3ab-4958-b5a5-0d3d281ab698)

3. At the bottom, this is info bar. It actually is carrying 3 funcitonalities. Firstly, as the figure above shown, it could instruct user of the available operation and corresponding keys to press. Secondly, it give user real-time exhibition of the content they are creating for certain task.
<img width="1108" alt="image" src="https://github.com/zhemp/Haskell-ToDo/assets/83204691/a6c771fe-912d-469d-855b-7f85e99cc16d">
Finally, it shows the error message when user do some unexpected operations.
<img width="1053" alt="image" src="https://github.com/zhemp/Haskell-ToDo/assets/83204691/3a5ff7cf-bb84-459a-af2d-6250bd913377">

### Challenges:
We have met loads of challenges during the development.
#### Challenges we solved: 
- The specification of the data structure.
- The functionality of enabling user to input string
- The implementation of deleting a whole task (delete main task will delete all corresponding subtasks)
- The development of the functionality of restore the done task and send it back to the priority list it came from.
- The implementation of saving the state when quitting and load the state when loading.
- Achievement of the complexed logic handling especially when create new main task and subtask so that the cursor will behave naturally
- The implementation of Theme
- The UI layout
- The cross over effect
- The effect of the sub task
- The implementation of ranking task

#### Challenges that still can be tried:
  muti-devices synchronization
   
### Goal accomplishment
We have nearly accomplished all the objectives we set at the project's outset, with the exception of network synchronization. This was due to our realization that such a feature is not particularly valuable for a terminal-based to-do list. Instead, we shifted our focus to implementing "themes" to enhance the visual appeal of the to-do list. I believe we have successfully achieved this revised goal, making the application more attractive and user-friendly.
## Progress at Dec 1
We finished the following tasks:  
- User Interface (UI) Development: The user interface has been defined and implemented, ensuring that users can interact effectively with the system. 
- Data Structure Implementation: A robust data structure has been created to store and index tasks. This structure is crucial for efficiently organizing, retrieving, and managing the tasks within the system, enhancing the overall functionality and user experience.
- Task Prioritization System: A system has been developed to prioritize tasks based on their urgency and importance. This feature allows for better management of tasks, ensuring that the most critical and time-sensitive tasks are addressed first.
- Task Addition Functionality: The functionality for adding new tasks has been implemented.
  
## Updated Milestones
### Milestone 2 (Week 10): Task Deletion, Modification and Sorting  
  - Edit Tasks: Facilitate modifications to the existing tasks.   
  - Delete Tasks: Provide the option to remove tasks that are no longer needed.   
  - Sorting Options: Allow tasks to be sorted based on their priority, due date, or other criteria.  

### Milestone 3 (Week 11): Data Export and Import  
- Objective: Provide flexibility in data management.  
- Features:  
  - Data Export: Allow users to export their to-do list for backup purposes.  
  - Data Import: Enable importing to-do lists from privious backups to ease the data transfer process.

### ~~Milestone 4 (If Time Allows): Synchronization and Accessibility~~ (Deprecated and modified to the theme milestne) 
- ~~Objective: Ensure data is accessible across multiple devices.~~ 
- ~~Features:~~  
  - ~~Syncing: Implement a synchronization mechanism to allow task data to be consistent across different devices.~~  
  - ~~Cross-Device Accessibility: Enable users to access their to-do list from various devices seamlessly.~~
  - 
### Milestone 4 (If Time Allows): Theme switch  
- Objective: Give user a better visual experience  
- Features:  
  - Theme switch: user can switch between several themes to pick the favorite one. 
  - Compatible with terminal of white or black background: The colors of different background shoule be carefully seleted such that no matter what background color of the user's terminal, he/she could select some themes have good visual effect.

## Original Projected Milestones (Deprecated)
### Milestone 1 (Week 8 - 9): Task Creation and Management
- Objective: Implement basic task management functionalities.  
- Features:  
  - Add Tasks: Users can add new tasks with essential details like title, description, and due date.  
  - Edit Tasks: Facilitate modifications to the existing tasks.  
  - Delete Tasks: Provide the option to remove tasks that are no longer needed.  
### Milestone 2 (Week 9-10): Prioritization
- Objective: Enhance the application with task prioritization features.  
- Features:  
  - Prioritization Mechanism: Implement a system (e.g., labeling or color-coding) to help users prioritize tasks.  
  - Sorting Options: Allow tasks to be sorted based on their priority, due date, or other criteria.  
### Milestone 3 (Week 10): Synchronization and Accessibility  
- Objective: Ensure data is accessible across multiple devices.  
- Features:  
  - Syncing: Implement a synchronization mechanism to allow task data to be consistent across different devices.  
  - Cross-Device Accessibility: Enable users to access their to-do list from various devices seamlessly.  
### Milestone 4 (If Time Allows): Data Export and Import  
- Objective: Provide flexibility in data management.  
- Features:  
  - Data Export: Allow users to export their to-do list for backup purposes.  
  - Data Import: Enable importing to-do lists from privious backups to ease the data transfer process.  
