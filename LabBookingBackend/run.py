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

    def get_booking(self):
        """Fetch and return the latest booking facts from Prolog."""
        try:
            # Query all current bookings
            bookings = list(self.prolog.query("booked(Room, Date, StartTime, EndTime, Person)."))
            # Optionally log or display these bookings if needed for debugging
            return bookings
        except Exception as e:
            messagebox.showerror("Error", f"Error fetching bookings: {str(e)}")
        return []
