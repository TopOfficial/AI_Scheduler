# AI_Scheduler

AI_Scheduler is a project designed to facilitate the booking and management of laboratories in the ECC building, as well as to create and manage class schedules. The project combines **Prolog** for logical reasoning and constraint handling, with **Python** for a user-friendly GUI implemented using **Tkinter**.

The project is structured into two main components:

---

## 1. **Backend Logic**

### **Prolog Integration**
- A folder containing Prolog files for defining lab booking rules, constraints, and scheduling logic. These include:
  - `Rooms.pl`: Contains information about room capacities.
  - `Preferences.pl`: Defines preferences for lecturers regarding rooms, days, and times.
  - `Lecturer.pl`: Lists lecturers along with their subjects and associated years.
  - `NumberOfStudents.pl`: Specifies the number of students per year.
  - `Scheduler.pl`: Implements the scheduling logic based on constraints and preferences.

### **Python-Prolog Bridge**
- A Python script that uses the `pyswip` library to interface with the Prolog engine. It executes Prolog queries and retrieves data such as schedules, available rooms, and lecturer preferences.

---

## 2. **Frontend/UI**

### **Laboratory Management**
- A set of Tkinter-based pages for booking and managing laboratory resources:
  - **RoomsFactsPage**: View and manage room data.
  - **AddRoomPage**: Add new room information.
  - **EditRoomPage**: Edit or delete existing room data.

### **Preferences Management**
- Tools for managing lecturer preferences for scheduling:
  - **PreferencesFactsPage**: View and manage lecturer preferences.
  - **AddPreferencesPage**: Add preferences for specific lecturers.
  - **EditPreferencesPage**: Edit or delete existing preferences.

---

## 3. **Class Scheduling**

### **Overview**
- The class scheduling functionality is designed to create and visualize schedules based on predefined constraints, such as room availability, lecturer preferences, and the number of students per year.

### **Features**
- **Schedule Visualization**:
  - Displays the generated schedule in a user-friendly grid layout.
  - Includes time slots (e.g., `morning`, `afternoon`) and days of the week (`Monday` to `Friday`).
  - Assigns colors to different subjects for better visual distinction.
  - Ensures readability by dynamically adjusting text contrast based on background color.

- **Dynamic Timetable Generation**:
  - Fetches and processes data using Prolog's reasoning engine.
  - Considers preferences for lecturers, rooms, and times while resolving conflicts.
  - Handles overlapping schedules and ensures no double-booking of rooms or lecturers.

- **Interactive Year Selection**:
  - Allows users to view schedules for specific years (e.g., Year 1 to Year 4) via a dropdown menu.

### **Pages**
- **ClassSchedulePage**:
  - Displays the schedule in a grid layout with time slots as rows and days as columns.
  - Adds dynamic elements such as lunch breaks and tooltips for room and lecturer details.

---

## Getting Started

### **Requirements**
- Python 3.7+
- `pyswip` library for Python-Prolog interaction.
- Tkinter (included with most Python installations).
- A Prolog interpreter (e.g., SWI-Prolog).

### **Setup**
1. Clone the repository:
   ```bash
   git clone https://github.com/your-repo/AI_Scheduler.git
