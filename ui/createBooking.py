import tkinter as tk
from tkinter import messagebox
from tkcalendar import Calendar
from reactButton import RectButton
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from LabBookingBackend.run import LabRoomBookingSystem
from datetime import datetime

PROLOG_PATH = "LabBookingBackend/labRoomBooking.pl"
ROOM_DEFINITIONS_PATH = "LabBookingBackend/roomDefinitions.pl"
RECORDS_PATH = "LabBookingBackend/roomBookedFacts.pl"
system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)


class CreateBooking(tk.Frame):
    def __init__(self, parent, controller=None):
        super().__init__(parent)
        self.controller = controller
        self.configure(bg="#e8f7f8")  # Light blue background

        self.width = 1440
        self.height = 1024
        self.pack_propagate(False)

        self.back_button = RectButton(
            self, 
            text="← BACK", 
            command=self.go_back, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.back_button.place(x=20, y=20)

        self.header_label = tk.Label(
            self, text="Create Booking", font=("Poppins", 30, "bold"), bg=self["bg"], fg="#17252A"
        )
        self.header_label.place(relx=0.5, rely=0.175, anchor="center")

        self.form_frame = tk.Frame(self, bg="#fff", padx=10, pady=10, borderwidth=1, relief="solid")
        self.form_frame.pack_propagate(False)
        self.form_frame.place(relx=0.5, rely=0.25, anchor="n")

        self.form_data = self.create_form()

        self.create_button = RectButton(
            self, 
            text="CREATE", 
            command=self.process_booking, 
            width=120, 
            height=40, 
            bg_color="#17252A",  
            fg_color="#FEFFFF", 
            font=("Poppins", 12, "bold")
        )
        self.create_button.place(relx=0.5, y=700, anchor="center")

    def create_form(self):
        rooms = system.fetch_rooms()

        tk.Label(self.form_frame, text="Select Room:", font=("Poppins", 12), bg="#fff").grid(row=0, column=0, padx=10, pady=5, sticky="w")
        room_var = tk.StringVar(self.form_frame)
        room_var.set(rooms[0] if rooms else "No Rooms Available")
        room_dropdown = tk.OptionMenu(self.form_frame, room_var, *rooms)
        room_dropdown.grid(row=0, column=1, padx=10, pady=5, sticky="ew")

        tk.Label(self.form_frame, text="Select Date:", font=("Poppins", 12), bg="#fff").grid(row=1, column=0, padx=10, pady=5, sticky="w")

        # Use `mindate` to restrict to today's date or later
        today = datetime.now().date()
        cal = Calendar(self.form_frame, date_pattern="yyyy-mm-dd", mindate=today)
        cal.grid(row=1, column=1, padx=10, pady=5, sticky="ew")

        start_time_entry = self.create_label_entry("Enter Start Time (e.g., '10:00'):", 2)
        end_time_entry = self.create_label_entry("Enter End Time (e.g., '12:00'):", 3)
        people_entry = self.create_label_entry("Enter Number of People:", 4)
        name_entry = self.create_label_entry("Enter Your Name:", 5)

        self.form_frame.columnconfigure(0, weight=1)
        self.form_frame.columnconfigure(1, weight=2)

        return {
            "room_var": room_var,
            "calendar": cal,
            "start_time_entry": start_time_entry,
            "end_time_entry": end_time_entry,
            "people_entry": people_entry,
            "name_entry": name_entry,
        }

    def create_label_entry(self, text, row):
        tk.Label(self.form_frame, text=text, font=("Poppins", 12), bg="#fff").grid(row=row, column=0, padx=10, pady=5, sticky="w")
        entry = tk.Entry(self.form_frame, font=("Poppins", 12), bg="#fff", fg="#000")
        entry.grid(row=row, column=1, padx=10, pady=5, sticky="ew")
        return entry

    def go_back(self):
        if self.controller:
            self.controller.show_frame("HomePage")

        else:
            print("Back button pressed (no controller linked)")

    def process_booking(self):
        form_data = self.form_data
        room = form_data["room_var"].get()
        date = form_data["calendar"].get_date()
        start_time = form_data["start_time_entry"].get()
        end_time = form_data["end_time_entry"].get()
        people_count = form_data["people_entry"].get()
        person_name = form_data["name_entry"].get()

        if not (room and date and start_time and end_time and people_count and person_name):
            messagebox.showerror("Error", "All fields are required.")
            return

        query_check = f"overlaps_booking({room}, '{date}', '{start_time}', '{end_time}')."
        try:
            overlaps = list(system.prolog.query(query_check))

            if overlaps:
                query_suggest = f"suggest_alternative_room({room}, '{date}', '{start_time}', '{end_time}', SuggestedRoom)."
                suggestions = list(system.prolog.query(query_suggest))

                if suggestions:
                    suggested_rooms = [s['SuggestedRoom'] for s in suggestions]
                    self.show_alternative_window(suggested_rooms, room, date, start_time, end_time, people_count, person_name)
                    return

            self.open_confirmation_window(room, date, start_time, end_time, people_count, person_name)

        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def show_alternative_window(self, suggested_rooms, room, date, start_time, end_time, people_count, person_name):
        alt_room_window = tk.Toplevel(self)
        alt_room_window.title("Choose Alternative Room or Time")

        tk.Label(
            alt_room_window,
            text="The requested room is unavailable. \nChoose an alternative room or retry another time:"
        ).pack(pady=10)

        alt_room_var = tk.StringVar(alt_room_window)
        alt_room_var.set(suggested_rooms[0])

        alt_room_dropdown = tk.OptionMenu(alt_room_window, alt_room_var, *suggested_rooms)
        alt_room_dropdown.pack(pady=10)

        def retry_another_time():
            alt_room_window.destroy()
            self.open_time_change_window()

        def confirm_alt_room():
            nonlocal room
            room = alt_room_var.get()
            alt_room_window.destroy()
            self.open_confirmation_window(room, date, start_time, end_time, people_count, person_name)

        tk.Button(alt_room_window, text="Retry Another Time", command=retry_another_time).pack(pady=10)
        tk.Button(alt_room_window, text="Confirm Room", command=confirm_alt_room).pack(pady=10)

    def open_confirmation_window(self, room, date, start_time, end_time, people_count, person_name):
        confirm_window = tk.Toplevel(self)
        confirm_window.title("Confirm Booking")
        summary_text = f"""
        Room: {room}
        Date: {date}
        Start Time: {start_time}
        End Time: {end_time}
        Number of People: {people_count}
        Name: {person_name}
        """
        tk.Label(confirm_window, text="Please confirm your booking details:").pack(pady=10)
        tk.Label(confirm_window, text=summary_text, justify="left").pack(pady=10)

        def confirm_booking():
            confirm_window.destroy()
            self.complete_booking(room, date, start_time, end_time, people_count, person_name)

        tk.Button(confirm_window, text="Confirm", command=confirm_booking).pack(pady=10)
        tk.Button(confirm_window, text="Cancel", command=confirm_window.destroy).pack(pady=10)

    def complete_booking(self, room, date, start_time, end_time, people_count, person_name):
        query = f"book_lab_room({room}, '{date.split('-')[2]}', '{date.split('-')[1]}', '{date.split('-')[0]}', '{start_time}', '{end_time}', {people_count}, '{person_name}')."
        try:
            list(system.prolog.query(query))
            self.record_booking(room, date, start_time, end_time, person_name)
            messagebox.showinfo("Success", f"Booking processed for room {room}.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    
    def open_time_change_window(self):
        """Open a window to change the start and end times."""
        time_window = tk.Toplevel(self)
        time_window.title("Change Time")

        tk.Label(time_window, text="Enter New Start Time (e.g., '10:00'):").grid(row=0, column=0, padx=10, pady=5)
        new_start_time_entry = tk.Entry(time_window)
        new_start_time_entry.grid(row=0, column=1, padx=10, pady=5)

        tk.Label(time_window, text="Enter New End Time (e.g., '12:00'):").grid(row=1, column=0, padx=10, pady=5)
        new_end_time_entry = tk.Entry(time_window)
        new_end_time_entry.grid(row=1, column=1, padx=10, pady=5)

        def confirm_new_times():
            room = self.form_data["room_var"].get()
            date = self.form_data["calendar"].get_date()
            people_count = self.form_data["people_entry"].get()
            person_name = self.form_data["name_entry"].get()

            new_start_time = new_start_time_entry.get()
            new_end_time = new_end_time_entry.get()

            if not (room and date and people_count and person_name):
                messagebox.showerror("Error", "All fields are required.")
                return

            if not (new_start_time and new_end_time):
                messagebox.showerror("Error", "Start and end times are required.")
                return

            time_window.destroy()
            self.open_confirmation_window_time(room, date, new_start_time, new_end_time, people_count, person_name)

        tk.Button(time_window, text="Confirm", command=confirm_new_times).grid(row=2, column=0, columnspan=2, pady=10)
        tk.Button(time_window, text="Cancel", command=time_window.destroy).grid(row=3, column=0, columnspan=2, pady=10)

    def open_confirmation_window_time(self, room, date, new_start_time, new_end_time, people_count, person_name):
        """Open a window to confirm booking details with new times."""
        confirm_window = tk.Toplevel(self)
        confirm_window.title("Confirm Booking")

        summary_text = f"""
        Room: {room}
        Date: {date}
        Start Time: {new_start_time}
        End Time: {new_end_time}
        Number of People: {people_count}
        Name: {person_name}
        """
        tk.Label(confirm_window, text="Please confirm your booking details:").pack(pady=10)
        tk.Label(confirm_window, text=summary_text, justify="left").pack(pady=10)

        def confirm_booking():
            confirm_window.destroy()
            self.complete_booking_time(room, date, new_start_time, new_end_time, people_count, person_name)

        tk.Button(confirm_window, text="Confirm", command=confirm_booking).pack(pady=10)
        tk.Button(confirm_window, text="Cancel", command=confirm_window.destroy).pack(pady=10)

    def complete_booking_time(self, room, date, start_time, end_time, people_count, person_name):
        """Finalize booking with new start and end times."""
        query = f"book_lab_room({room}, '{date.split('-')[2]}', '{date.split('-')[1]}', '{date.split('-')[0]}', '{start_time}', '{end_time}', {people_count}, '{person_name}')."
        try:
            list(system.prolog.query(query))
            self.record_booking(room, date, start_time, end_time, person_name)
            messagebox.showinfo("Success", f"Booking processed for room {room}.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")
            
            
    def record_booking(self, room, date, start_time, end_time, person_name):
        """Record the booking in the roomBookedFacts.pl file."""
        def format_time(time_str):
            """
            Formats a time string to HH:MM format in 24-hour format.
            Supports input formats with ':' or '.' as the separator.
            """
            # Replace '.' with ':' to normalize the input
            normalized_time_str = time_str.replace('.', ':')
            
            try:
                # Parse time string into datetime object
                time_obj = datetime.strptime(normalized_time_str, "%H:%M")
            except ValueError:
                try:
                    # Handle cases like '8', '14' without minutes
                    time_obj = datetime.strptime(normalized_time_str, "%H")
                except ValueError:
                    raise ValueError(f"Invalid time format: {time_str}")
            return time_obj.strftime("%H:%M")

        try:
            # Format start and end times
            formatted_start_time = format_time(start_time)
            formatted_end_time = format_time(end_time)

            # Create the booking fact
            full_date = date
            booking_fact = f"booked({room}, '{full_date}', '{formatted_start_time}', '{formatted_end_time}', '{person_name}').\n"

            # Write the booking fact to the file
            with open(RECORDS_PATH, 'a') as file:
                file.write(booking_fact)

        except ValueError as ve:
            messagebox.showerror("Error", f"Error with time format: {str(ve)}")
        except Exception as e:
            messagebox.showerror("Error", f"Error recording booking: {str(e)}")