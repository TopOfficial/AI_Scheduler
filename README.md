# AI_Scheduler

AI_Scheduler is a project designed to facilitate the booking and management of laboratory of ECC building. The project combines **Prolog** for logical reasoning and constraints handling, with **Python** for a user-friendly GUI implemented using **Tkinter**. 

The project is structured into two main components:

1. **Backend Logic**:
   - A folder containing Prolog files for defining lab booking rules and constraints.
   - A Python script that uses the `pyswip` library to interact with the Prolog logic.

2. **Frontend/UI**:
   - A collection of Python files implementing various pages using the Tkinter library.
   - Includes a `main.py` file to launch the application.

---

## Project Structure

```plaintext
LabBookingBackend/
├── backend/
│   ├── lab_logic.pl          # Prolog logic for lab bookings
│   ├── prolog_interface.py   # Python script to call Prolog logic
│
├── ui/
│   ├── main.py               # Entry point to run the application
│   ├── startpage.py          # UI for the start page
│   ├── homepage.py           # UI for the home page
│   ├── createBooking.py      # UI to create a new booking
│   ├── viewbookingpage.py    # UI to view existing bookings
│   ├── labdetailpage.py      # UI to view lab details
│   ├── labLayoutPage.py      # UI for lab layout visualization
│
├── README.md                 # Project documentation
