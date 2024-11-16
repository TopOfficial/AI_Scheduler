from pyswip import Prolog

class LabRoomBookingSystem:
    def __init__(self, prolog_path, room_definitions_path, records_path):
        self.prolog = Prolog()
        self.prolog_path = prolog_path
        self.room_definitions_path = room_definitions_path
        self.records_path = records_path
        self.prolog.consult(prolog_path)
        self.prolog.consult(room_definitions_path)

    def main_menu(self):
        while True:
            print("\n--- Lab Room Booking System ---")
            print("1. Book a Room")
            print("2. View All Bookings")
            print("3. Delete a Booking")
            print("4. Search Bookings by Date")
            print("5. Search Bookings by Room")
            print("6. Search Bookings by Person")
            print("7. Filter Bookings by Date, Room, and Person")
            print("8. Edit a Booking")
            print("9. Add a Room")
            print("10. Exit")
            
            choice = input("Enter your choice: ")
            
            if choice == "1":
                self.book_room()
            elif choice == "2":
                self.view_bookings()
            elif choice == "3":
                self.delete_booking()
            elif choice == "4":
                self.search_by_date()
            elif choice == "5":
                self.search_by_room()
            elif choice == "6":
                self.search_by_person()
            elif choice == "7":
                self.filter_bookings()
            elif choice == "8":
                self.edit_booking()
            elif choice == "9":
                self.add_room()
            elif choice == "10":
                print("Goodbye!")
                break
            else:
                print("Invalid choice. Please try again.")

    def book_room(self):
        print("\n--- Book a Room ---")
        room = input("Enter room name (e.g., lab1, lab2): ")
        date = input("Enter day (DD): ")
        month = input("Enter month (MM): ")
        year = input("Enter year (YYYY): ")
        time = input("Enter time slot (e.g., '10:00'): ")
        people_count = int(input("Enter number of people: "))
        preferred_slot = input("Enter preferred slot (morning/afternoon): ")
        person_name = input("Enter your name: ")
        
        query = f"book_lab_room({room}, {date}, {month}, {year}, '{time}', {people_count}, {preferred_slot}, '{person_name}')."
        try:
            list(self.prolog.query(query))
            print("Booking processed.")
            self.record_booking(room, date, month, year, time, person_name)
        except Exception as e:
            print(f"Error: {str(e)}")

    def view_bookings(self):
        print("\n--- All Bookings ---")
        query = "list_all_bookings."
        try:
            list(self.prolog.query(query))
        except Exception as e:
            print(f"Error: {str(e)}")

    def delete_booking(self):
        print("\n--- Delete a Booking ---")
        room = input("Enter room name (e.g., lab1, lab2): ")
        date = input("Enter full date (YYYY-MM-DD): ")
        time = input("Enter time slot (e.g., '10:00'): ")
        person_name = input("Enter the name of the person who booked: ")
        
        query = f"delete_booking({room}, '{date}', '{time}', '{person_name}')."
        try:
            list(self.prolog.query(query))
            print("Booking deleted successfully.")
        except Exception as e:
            print(f"Error: {str(e)}")

    def search_by_date(self):
        print("\n--- Search Bookings by Date ---")
        date = input("Enter full date (YYYY-MM-DD): ")
        query = f"search_bookings_by_date('{date}')."
        try:
            list(self.prolog.query(query))
        except Exception as e:
            print(f"Error: {str(e)}")

    def search_by_room(self):
        print("\n--- Search Bookings by Room ---")
        room = input("Enter room name (e.g., lab1, lab2): ")
        query = f"search_bookings_by_room({room})."
        try:
            list(self.prolog.query(query))
        except Exception as e:
            print(f"Error: {str(e)}")

    def search_by_person(self):
        print("\n--- Search Bookings by Person ---")
        person = input("Enter person name: ")
        query = f"search_bookings_by_person('{person}')."
        try:
            list(self.prolog.query(query))
        except Exception as e:
            print(f"Error: {str(e)}")

    def filter_bookings(self):
        print("\n--- Filter Bookings ---")
        date = input("Enter full date (YYYY-MM-DD): ")
        room = input("Enter room name (e.g., lab1, lab2): ")
        person = input("Enter person name: ")
        query = f"search_bookings('{date}', {room}, '{person}')."
        try:
            list(self.prolog.query(query))
        except Exception as e:
            print(f"Error: {str(e)}")

    def edit_booking(self):
        print("\n--- Edit a Booking ---")
        old_room = input("Enter old room name (e.g., lab1, lab2): ")
        old_date = input("Enter old full date (YYYY-MM-DD): ")
        old_time = input("Enter old time slot (e.g., '10:00'): ")
        old_person = input("Enter old person name: ")
        new_room = input("Enter new room name (e.g., lab1, lab2): ")
        new_date = input("Enter new full date (YYYY-MM-DD): ")
        new_time = input("Enter new time slot (e.g., '10:00'): ")
        new_person = input("Enter new person name: ")
        
        query = f"edit_booking({old_room}, '{old_date}', '{old_time}', '{old_person}', {new_room}, '{new_date}', '{new_time}', '{new_person}')."
        try:
            list(self.prolog.query(query))
            print("Booking edited successfully.")
        except Exception as e:
            print(f"Error: {str(e)}")

    def add_room(self):
        print("\n--- Add a Room ---")
        room_name = input("Enter room name (e.g., lab4): ")
        building_name = input("Enter building name (e.g., ecc): ")
        capacity = int(input("Enter room capacity (e.g., 40): "))
        
        # Append the new room to roomDefinitions.pl
        room_fact = f"room({room_name}, {building_name}, {capacity}).\n"
        try:
            with open(self.room_definitions_path, 'a') as file:
                file.write(room_fact)
            print(f"Room {room_name} added successfully.")
        except Exception as e:
            print(f"Error: {str(e)}")

    def record_booking(self, room, date, month, year, time, person_name):
        """Record the booking in the roomBookedFacts.pl file."""
        full_date = f"{year}-{month}-{date}"
        booking_fact = f"booked({room}, '{full_date}', '{time}', '{person_name}').\n"
        try:
            with open(self.records_path, 'a') as file:
                file.write(booking_fact)
            print(f"Booking recorded in {self.records_path}.")
        except Exception as e:
            print(f"Error recording booking: {str(e)}")

# Paths to Prolog files
PROLOG_PATH = "LabBookingBackend/labRoomBooking.pl"
ROOM_DEFINITIONS_PATH = "LabBookingBackend/roomDefinitions.pl"
RECORDS_PATH = "LabBookingBackend/roomBookedFacts.pl"

if __name__ == "__main__":
    system = LabRoomBookingSystem(PROLOG_PATH, ROOM_DEFINITIONS_PATH, RECORDS_PATH)
    system.main_menu()
