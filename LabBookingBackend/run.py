# from pyswip import Prolog

# class LabRoomBookingSystem:
#     def __init__(self, prolog_path, room_definitions_path, records_path):
#         self.prolog = Prolog()
#         self.prolog_path = prolog_path
#         self.room_definitions_path = room_definitions_path
#         self.records_path = records_path
#         self.prolog.consult(prolog_path)
#         self.prolog.consult(room_definitions_path)

#     def main_menu(self):
#         while True:
#             print("\n--- Lab Room Booking System ---")
#             print("1. Book a Room")
#             print("2. View All Bookings")
#             print("3. Delete a Booking")
#             print("4. Search Bookings by Date")
#             print("5. Search Bookings by Room")
#             print("6. Search Bookings by Person")
#             print("7. Filter Bookings by Date, Room, and Person")
#             print("8. Edit a Booking")
#             print("9. Add a Room")
#             print("10. Exit")
            
#             choice = input("Enter your choice: ")
            
#             if choice == "1":
#                 self.book_room()
#             elif choice == "2":
#                 self.view_bookings()
#             elif choice == "3":
#                 self.delete_booking()
#             elif choice == "4":
#                 self.search_by_date()
#             elif choice == "5":
#                 self.search_by_room()
#             elif choice == "6":
#                 self.search_by_person()
#             elif choice == "7":
#                 self.filter_bookings()
#             elif choice == "8":
#                 self.edit_booking()
#             elif choice == "9":
#                 self.add_room()
#             elif choice == "10":
#                 print("Goodbye!")
#                 break
#             else:
#                 print("Invalid choice. Please try again.")

#     def book_room(self):
#         print("\n--- Book a Room ---")
#         room = input("Enter room name (e.g., lab1, lab2): ")
#         date = input("Enter day (DD): ")
#         month = input("Enter month (MM): ")
#         year = input("Enter year (YYYY): ")
#         time = input("Enter time slot (e.g., '10:00'): ")
#         people_count = int(input("Enter number of people: "))
#         preferred_slot = input("Enter preferred slot (morning/afternoon): ")
#         person_name = input("Enter your name: ")
        
#         query = f"book_lab_room({room}, {date}, {month}, {year}, '{time}', {people_count}, {preferred_slot}, '{person_name}')."
#         try:
#             list(self.prolog.query(query))
#             print("Booking processed.")
#             self.record_booking(room, date, month, year, time, person_name)
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def view_bookings(self):
#         print("\n--- All Bookings ---")
#         query = "list_all_bookings."
#         try:
#             list(self.prolog.query(query))
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def delete_booking(self):
#         print("\n--- Delete a Booking ---")
#         room = input("Enter room name (e.g., lab1, lab2): ")
#         date = input("Enter full date (YYYY-MM-DD): ")
#         time = input("Enter time slot (e.g., '10:00'): ")
#         person_name = input("Enter the name of the person who booked: ")
        
#         query = f"delete_booking({room}, '{date}', '{time}', '{person_name}')."
#         try:
#             list(self.prolog.query(query))
#             print("Booking deleted successfully.")
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def search_by_date(self):
#         print("\n--- Search Bookings by Date ---")
#         date = input("Enter full date (YYYY-MM-DD): ")
#         query = f"search_bookings_by_date('{date}')."
#         try:
#             list(self.prolog.query(query))
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def search_by_room(self):
#         print("\n--- Search Bookings by Room ---")
#         room = input("Enter room name (e.g., lab1, lab2): ")
#         query = f"search_bookings_by_room({room})."
#         try:
#             list(self.prolog.query(query))
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def search_by_person(self):
#         print("\n--- Search Bookings by Person ---")
#         person = input("Enter person name: ")
#         query = f"search_bookings_by_person('{person}')."
#         try:
#             list(self.prolog.query(query))
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def filter_bookings(self):
#         print("\n--- Filter Bookings ---")
#         date = input("Enter full date (YYYY-MM-DD): ")
#         room = input("Enter room name (e.g., lab1, lab2): ")
#         person = input("Enter person name: ")
#         query = f"search_bookings('{date}', {room}, '{person}')."
#         try:
#             list(self.prolog.query(query))
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def edit_booking(self):
#         print("\n--- Edit a Booking ---")
#         old_room = input("Enter old room name (e.g., lab1, lab2): ")
#         old_date = input("Enter old full date (YYYY-MM-DD): ")
#         old_time = input("Enter old time slot (e.g., '10:00'): ")
#         old_person = input("Enter old person name: ")
#         new_room = input("Enter new room name (e.g., lab1, lab2): ")
#         new_date = input("Enter new full date (YYYY-MM-DD): ")
#         new_time = input("Enter new time slot (e.g., '10:00'): ")
#         new_person = input("Enter new person name: ")
        
#         query = f"edit_booking({old_room}, '{old_date}', '{old_time}', '{old_person}', {new_room}, '{new_date}', '{new_time}', '{new_person}')."
#         try:
#             list(self.prolog.query(query))
#             print("Booking edited successfully.")
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def add_room(self):
#         print("\n--- Add a Room ---")
#         room_name = input("Enter room name (e.g., lab4): ")
#         building_name = input("Enter building name (e.g., ecc): ")
#         capacity = int(input("Enter room capacity (e.g., 40): "))
        
#         # Append the new room to roomDefinitions.pl
#         room_fact = f"room({room_name}, {building_name}, {capacity}).\n"
#         try:
#             with open(self.room_definitions_path, 'a') as file:
#                 file.write(room_fact)
#             print(f"Room {room_name} added successfully.")
#         except Exception as e:
#             print(f"Error: {str(e)}")

#     def record_booking(self, room, date, month, year, time, person_name):
#         """Record the booking in the roomBookedFacts.pl file."""
#         full_date = f"{year}-{month}-{date}"
#         booking_fact = f"booked({room}, '{full_date}', '{time}', '{person_name}').\n"
#         try:
#             with open(self.records_path, 'a') as file:
#                 file.write(booking_fact)
#             print(f"Booking recorded in {self.records_path}.")
#         except Exception as e:
#             print(f"Error recording booking: {str(e)}")

# # Paths to Prolog files
# PROLOG_PATH = "LabBookingBackend/labRoomBooking.pl"
# ROOM_DEFINITIONS_PATH = "LabBookingBackend/roomDefinitions.pl"
# RECORDS_PATH = "LabBookingBackend/roomBookedFacts.pl"

# if __name__ == "__main__":
#     system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)
#     system.main_menu()

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

import tkinter as tk
from tkinter import messagebox, simpledialog
from tkcalendar import Calendar
from pyswip import Prolog

class LabRoomBookingSystem:
    def __init__(self, prolog_path, room_definitions_path, records_path):
        self.prolog = Prolog()
        self.prolog_path = prolog_path
        self.room_definitions_path = room_definitions_path
        self.records_path = records_path
        self.prolog.consult(prolog_path)
        self.prolog.consult(room_definitions_path)

        # Fetch room options from Prolog
        self.rooms = self.fetch_rooms()

        # Initialize Tkinter window
        self.root = tk.Tk()
        self.root.title("Lab Room Booking System")

        # Build the menu
        self.build_menu()
        
    def fetch_rooms(self):
        """Fetch room names from roomDefinitions.pl."""
        try:
            return [str(room["X"]) for room in self.prolog.query("room(X, _, _).")]
        except Exception as e:
            messagebox.showerror("Error", f"Error fetching rooms: {str(e)}")
            return []

    def fetch_booking(self):
        """Fetch the latest booking facts from Prolog."""
        try:
            # Query all current bookings
            bookings = list(self.prolog.query("booked(Room, Date, StartTime, EndTime, Person)."))
            # Optionally log or display these bookings if needed for debugging
            for booking in bookings:
                print(f"Booking: Room {booking['Room']}, Date {booking['Date']}, Time {booking['StartTime']}-{booking['EndTime']}, Person: {booking['Person']}")
        except Exception as e:
            messagebox.showerror("Error", f"Error fetching bookings: {str(e)}")


    def build_menu(self):
        tk.Label(self.root, text="Lab Room Booking System", font=("Helvetica", 16)).pack(pady=10)
        buttons = [
            ("Book a Room", self.book_room),
            ("View All Bookings", self.view_bookings),
            ("Delete a Booking", self.delete_booking),
            ("Search Bookings by Date", self.search_by_date),
            ("Search Bookings by Room", self.search_by_room),
            ("Search Bookings by Person", self.search_by_person),
            ("Filter Bookings by Date, Room, and Person", self.filter_bookings),
            ("Edit Booking", self.edit_booking),
            ("Add a Room", self.add_room),
            ("Exit", self.exit_system),
        ]

        for text, command in buttons:
            tk.Button(self.root, text=text, command=command, width=30, height=2).pack(pady=5)

    def book_room(self):
        booking_window = tk.Toplevel(self.root)
        booking_window.title("Book a Room")

        tk.Label(booking_window, text="Select Room:").grid(row=0, column=0, padx=10, pady=5)
        room_var = tk.StringVar(booking_window)
        room_var.set(self.rooms[0] if self.rooms else "No Rooms Available")
        room_dropdown = tk.OptionMenu(booking_window, room_var, *self.rooms)
        room_dropdown.grid(row=0, column=1, padx=10, pady=5)

        tk.Label(booking_window, text="Select Date:").grid(row=1, column=0, padx=10, pady=5)
        cal = Calendar(booking_window, date_pattern="yyyy-mm-dd")
        cal.grid(row=1, column=1, padx=10, pady=5)

        tk.Label(booking_window, text="Enter Start Time (e.g., '10:00'):").grid(row=2, column=0, padx=10, pady=5)
        start_time_entry = tk.Entry(booking_window)
        start_time_entry.grid(row=2, column=1, padx=10, pady=5)

        tk.Label(booking_window, text="Enter End Time (e.g., '12:00'):").grid(row=3, column=0, padx=10, pady=5)
        end_time_entry = tk.Entry(booking_window)
        end_time_entry.grid(row=3, column=1, padx=10, pady=5)

        tk.Label(booking_window, text="Enter Number of People:").grid(row=4, column=0, padx=10, pady=5)
        people_entry = tk.Entry(booking_window)
        people_entry.grid(row=4, column=1, padx=10, pady=5)

        tk.Label(booking_window, text="Enter Your Name:").grid(row=5, column=0, padx=10, pady=5)
        name_entry = tk.Entry(booking_window)
        name_entry.grid(row=5, column=1, padx=10, pady=5)

        def process_booking():
            self.fetch_booking()

            room = room_var.get()
            date = cal.get_date()
            start_time = start_time_entry.get()
            end_time = end_time_entry.get()
            people_count = people_entry.get()
            person_name = name_entry.get()

            if not (room and date and start_time and end_time and people_count and person_name):
                messagebox.showerror("Error", "All fields are required.")
                return

            query_check = f"overlaps_booking({room}, '{date}', '{start_time}', '{end_time}')."
            try:
                overlaps = list(self.prolog.query(query_check))

                if overlaps:
                    query_suggest = f"suggest_alternative_room({room}, '{date}', '{start_time}', '{end_time}', SuggestedRoom)."
                    suggestions = list(self.prolog.query(query_suggest))

                    if suggestions:
                        suggested_rooms = [s['SuggestedRoom'] for s in suggestions]

                        alt_room_window = tk.Toplevel(self.root)
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
                            open_time_change_window()

                        def confirm_alt_room():
                            nonlocal room
                            room = alt_room_var.get()
                            print(f"Selected alternative room: {room}")
                            alt_room_window.destroy()
                            open_confirmation_window(room, date, start_time, end_time, people_count, person_name)

                        tk.Button(alt_room_window, text="Retry Another Time", command=retry_another_time).pack(pady=10)
                        tk.Button(alt_room_window, text="Confirm Room", command=confirm_alt_room).pack(pady=10)
                        return
                    
                open_confirmation_window(room, date, start_time, end_time, people_count, person_name)

            except Exception as e:messagebox.showerror("Error", f"Error: {str(e)}")
            
        def open_time_change_window():
            """Open a window to change the start and end times."""
            time_window = tk.Toplevel(self.root)
            time_window.title("Change Time")

            tk.Label(time_window, text="Enter New Start Time (e.g., '10:00'):").grid(row=0, column=0, padx=10, pady=5)
            new_start_time_entry = tk.Entry(time_window)
            new_start_time_entry.grid(row=0, column=1, padx=10, pady=5)

            tk.Label(time_window, text="Enter New End Time (e.g., '12:00'):").grid(row=1, column=0, padx=10, pady=5)
            new_end_time_entry = tk.Entry(time_window)
            new_end_time_entry.grid(row=1, column=1, padx=10, pady=5)

            def confirm_new_times():
                room = room_var.get()
                date = cal.get_date()
                people_count = people_entry.get()
                person_name = name_entry.get()

                new_start_time = new_start_time_entry.get()
                new_end_time = new_end_time_entry.get()
                
                if not (room and date and people_count and person_name):
                    messagebox.showerror("Error", "All fields are required.")
                    return
                
                if not (new_start_time and new_end_time):
                    messagebox.showerror("Error", "Start and end times are required.")
                    return

                time_window.destroy()
                open_confirmation_window_time(room, date, new_start_time, new_end_time, people_count, person_name)

            tk.Button(time_window, text="Confirm", command=confirm_new_times).grid(row=2, column=0, columnspan=2, pady=10)
            tk.Button(time_window, text="Cancel", command=time_window.destroy).grid(row=3, column=0, columnspan=2, pady=10)

        def open_confirmation_window(room, date, start_time, end_time, people_count, person_name):
            """Open a window to confirm booking details."""
            confirm_window = tk.Toplevel(self.root)
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
                complete_booking(room)

            tk.Button(confirm_window, text="Confirm", command=confirm_booking).pack(pady=10)
            tk.Button(confirm_window, text="Cancel", command=confirm_window.destroy).pack(pady=10)
            
        def open_confirmation_window_time(room, date, new_start_time, new_end_time, people_count, person_name):
            """Open a window to confirm booking details."""
            confirm_window = tk.Toplevel(self.root)
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
                complete_booking_time(new_start_time, new_end_time)

            tk.Button(confirm_window, text="Confirm", command=confirm_booking).pack(pady=10)
            tk.Button(confirm_window, text="Cancel", command=confirm_window.destroy).pack(pady=10)
        
        def complete_booking_time(start_time, end_time):
            """Finalize booking with new start and end times."""
            room = room_var.get()
            date = cal.get_date()
            people_count = people_entry.get()
            person_name = name_entry.get()

            query = f"book_lab_room({room}, '{date.split('-')[2]}', '{date.split('-')[1]}', '{date.split('-')[0]}', '{start_time}', '{end_time}', {people_count}, '{person_name}')."
            try:
                list(self.prolog.query(query))
                self.record_booking(room, date.split('-')[2], date.split('-')[1], date.split('-')[0], start_time, end_time, person_name)
                messagebox.showinfo("Success", f"Booking processed for room {room}.")
                booking_window.destroy()
            except Exception as e:
                messagebox.showerror("Error", f"Error: {str(e)}")
        
        def complete_booking(room):
            """Finalize the booking process."""
            date = cal.get_date()
            start_time = start_time_entry.get()
            end_time = end_time_entry.get()
            people_count = people_entry.get()
            person_name = name_entry.get()

            query = f"book_lab_room({room}, '{date.split('-')[2]}', '{date.split('-')[1]}', '{date.split('-')[0]}', '{start_time}', '{end_time}', {people_count}, '{person_name}')."
            try:
                list(self.prolog.query(query))
                self.record_booking(room, date.split('-')[2], date.split('-')[1], date.split('-')[0], start_time, end_time, person_name)
                messagebox.showinfo("Success", f"Booking processed for room {room}.")
                booking_window.destroy()
            except Exception as e:
                messagebox.showerror("Error", f"Error: {str(e)}")


        tk.Button(booking_window, text="Submit", command=process_booking).grid(row=6, column=0, columnspan=2, pady=10)

    def record_booking(self, room, day, month, year, start_time, end_time, person_name):
        """Record the booking in the roomBookedFacts.pl file."""
        full_date = f"{year}-{month}-{day}"
        booking_fact = f"booked({room}, '{full_date}', '{start_time}', '{end_time}', '{person_name}').\n"
        try:
            with open(self.records_path, 'a') as file:
                file.write(booking_fact)
        except Exception as e:
            messagebox.showerror("Error", f"Error recording booking: {str(e)}")
            


    def view_bookings(self):
        query = "list_all_bookings."
        try:
            result = list(self.prolog.query(query))
            bookings = "\n".join([str(r) for r in result])
            messagebox.showinfo("All Bookings", bookings if bookings else "No bookings found.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def delete_booking(self):
        room = simpledialog.askstring("Delete a Booking", "Enter room name (e.g., lab1, lab2):")
        date = simpledialog.askstring("Delete a Booking", "Enter full date (YYYY-MM-DD):")
        time = simpledialog.askstring("Delete a Booking", "Enter time slot (e.g., '10:00'):")
        person_name = simpledialog.askstring("Delete a Booking", "Enter the name of the person who booked:")

        query = f"delete_booking({room}, '{date}', '{time}', '{person_name}')."
        try:
            list(self.prolog.query(query))
            messagebox.showinfo("Success", "Booking deleted successfully.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def search_by_date(self):
        date = simpledialog.askstring("Search by Date", "Enter full date (YYYY-MM-DD):")
        query = f"search_bookings_by_date('{date}')."
        try:
            result = list(self.prolog.query(query))
            bookings = "\n".join([str(r) for r in result])
            messagebox.showinfo("Search by Date", bookings if bookings else "No bookings found for the date.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def search_by_room(self):
        room = simpledialog.askstring("Search by Room", "Enter room name (e.g., lab1, lab2):")
        query = f"search_bookings_by_room({room})."
        try:
            result = list(self.prolog.query(query))
            bookings = "\n".join([str(r) for r in result])
            messagebox.showinfo("Search by Room", bookings if bookings else "No bookings found for the room.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def search_by_person(self):
        person = simpledialog.askstring("Search by Person", "Enter person name:")
        query = f"search_bookings_by_person('{person}')."
        try:
            result = list(self.prolog.query(query))
            bookings = "\n".join([str(r) for r in result])
            messagebox.showinfo("Search by Person", bookings if bookings else "No bookings found for the person.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def filter_bookings(self):
        date = simpledialog.askstring("Filter Bookings", "Enter full date (YYYY-MM-DD):")
        room = simpledialog.askstring("Filter Bookings", "Enter room name (e.g., lab1, lab2):")
        person = simpledialog.askstring("Filter Bookings", "Enter person name:")
        query = f"search_bookings('{date}', {room}, '{person}')."
        try:
            result = list(self.prolog.query(query))
            bookings = "\n".join([str(r) for r in result])
            messagebox.showinfo("Filter Bookings", bookings if bookings else "No bookings found matching the criteria.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def edit_booking(self):
        # Fetch all bookings
        try:
            bookings = list(self.prolog.query("booked(Room, Date, StartTime, EndTime, Person)."))
            if not bookings:
                messagebox.showinfo("No Bookings", "No bookings available to edit.")
                return
        except Exception as e:
            messagebox.showerror("Error", f"Error fetching bookings: {str(e)}")
            return

        # Open Edit Booking Window
        edit_window = tk.Toplevel(self.root)
        edit_window.title("Edit Booking")

        # Select Booking
        tk.Label(edit_window, text="Select Booking:").grid(row=0, column=0, padx=10, pady=5)
        booking_var = tk.StringVar(edit_window)
        booking_var.set("Select a booking")

        # Prepare dropdown options
        booking_options = [
            f"{b['Room']}, {b['Date']}, {b['StartTime']} to {b['EndTime']}, {b['Person']}"
            for b in bookings
        ]
        booking_dropdown = tk.OptionMenu(edit_window, booking_var, *booking_options)
        booking_dropdown.grid(row=0, column=1, padx=10, pady=5)

        # Form fields for editing
        tk.Label(edit_window, text="Room:").grid(row=1, column=0, padx=10, pady=5)
        room_entry = tk.Entry(edit_window)
        room_entry.grid(row=1, column=1, padx=10, pady=5)

        tk.Label(edit_window, text="Date (YYYY-MM-DD):").grid(row=2, column=0, padx=10, pady=5)
        date_entry = tk.Entry(edit_window)
        date_entry.grid(row=2, column=1, padx=10, pady=5)

        tk.Label(edit_window, text="Start Time:").grid(row=3, column=0, padx=10, pady=5)
        start_time_entry = tk.Entry(edit_window)
        start_time_entry.grid(row=3, column=1, padx=10, pady=5)

        tk.Label(edit_window, text="End Time:").grid(row=4, column=0, padx=10, pady=5)
        end_time_entry = tk.Entry(edit_window)
        end_time_entry.grid(row=4, column=1, padx=10, pady=5)

        tk.Label(edit_window, text="Person Name:").grid(row=5, column=0, padx=10, pady=5)
        person_entry = tk.Entry(edit_window)
        person_entry.grid(row=5, column=1, padx=10, pady=5)

        # Pre-fill form fields based on selected booking
        def prefill_form(*args):
            selected_index = booking_options.index(booking_var.get())
            selected_booking = bookings[selected_index]

            room_entry.delete(0, tk.END)
            room_entry.insert(0, selected_booking["Room"])

            date_entry.delete(0, tk.END)
            date_entry.insert(0, selected_booking["Date"])

            start_time_entry.delete(0, tk.END)
            start_time_entry.insert(0, selected_booking["StartTime"])

            end_time_entry.delete(0, tk.END)
            end_time_entry.insert(0, selected_booking["EndTime"])

            person_entry.delete(0, tk.END)
            person_entry.insert(0, selected_booking["Person"])

        booking_var.trace("w", prefill_form)

        # Save Edited Booking
        def save_changes():
            selected_index = booking_options.index(booking_var.get())
            old_booking = bookings[selected_index]

            new_room = room_entry.get()
            new_date = date_entry.get()
            new_start_time = start_time_entry.get()
            new_end_time = end_time_entry.get()
            new_person = person_entry.get()

            if not (new_room and new_date and new_start_time and new_end_time and new_person):
                messagebox.showerror("Error", "All fields are required.")
                return

            # Prolog query to edit the booking
            query = (
                f"edit_booking({old_booking['Room']}, '{old_booking['Date']}', "
                f"'{old_booking['StartTime']}', '{old_booking['EndTime']}', '{old_booking['Person']}', "
                f"{new_room}, '{new_date}', '{new_start_time}', '{new_end_time}', '{new_person}')."
            )
            try:
                list(self.prolog.query(query))
                messagebox.showinfo("Success", "Booking edited successfully.")
                edit_window.destroy()
            except Exception as e:
                messagebox.showerror("Error", f"Error: {str(e)}")

        tk.Button(edit_window, text="Save Changes", command=save_changes).grid(row=6, column=0, columnspan=2, pady=10)


    def add_room(self):
        room_name = simpledialog.askstring("Add Room", "Enter room name (e.g., lab4):")
        building_name = simpledialog.askstring("Add Room", "Enter building name (e.g., ecc):")
        capacity = simpledialog.askinteger("Add Room", "Enter room capacity (e.g., 40):")

        room_fact = f"room({room_name}, {building_name}, {capacity}).\n"
        try:
            with open(self.room_definitions_path, 'a') as file:
                file.write(room_fact)
            messagebox.showinfo("Success", f"Room {room_name} added successfully.")
        except Exception as e:
            messagebox.showerror("Error", f"Error: {str(e)}")

    def exit_system(self):
        self.root.destroy()

# Paths to Prolog files
PROLOG_PATH = "LabBookingBackend/labRoomBooking.pl"
ROOM_DEFINITIONS_PATH = "LabBookingBackend/roomDefinitions.pl"
RECORDS_PATH = "LabBookingBackend/roomBookedFacts.pl"

if __name__ == "__main__":
    system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)
    system.root.mainloop()