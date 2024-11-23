import tkinter as tk
from reactButton import RectButton
from tkinter import messagebox

class AddRoomPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'
        
        self.path = "ScheduleOrganiser/Rooms.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Add Room",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.1, anchor='center')

        # Room Name Label and Entry
        self.room_name_label = tk.Label(
            self,
            text="Room Name:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.room_name_label.place(relx=0.2, rely=0.3, anchor='w')

        self.room_name_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.room_name_entry.place(relx=0.4, rely=0.3, anchor='w')

        # Capacity Label and Entry
        self.capacity_label = tk.Label(
            self,
            text="Capacity:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.capacity_label.place(relx=0.2, rely=0.4, anchor='w')

        self.capacity_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.capacity_entry.place(relx=0.4, rely=0.4, anchor='w')

        # Add Room Button
        self.add_button = RectButton(
            self,
            text="Add Room",
            command=self.add_room,
            width=140,
            height=40,
            bg_color="#0F6004",  # Green button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.add_button.place(relx=0.5, rely=0.6, anchor='center')

        # Back Button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=lambda: controller.show_frame("RoomsFactsPage"),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

    def add_room(self):
        """Add the room to the Prolog knowledge base."""
        room_name = self.room_name_entry.get().strip()
        capacity = self.capacity_entry.get().strip()

        if room_name and capacity.isdigit():
            capacity = int(capacity)
            fact = f"room('{room_name}', {capacity})"

            try:
                # Read the file to check if the room name already exists
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                # Extract room names from existing facts and check for duplicates
                existing_room_names = [
                    line.split('(')[1].split(',')[0].strip("'")  # Extract room name part
                    for line in existing_facts
                    if line.startswith("room(")  # Ensure it's a room fact
                ]

                if room_name in existing_room_names:
                    # Show error dialog for duplicate room name
                    messagebox.showerror("Duplicate Entry", f"Room '{room_name}' already exists.")
                    return  # Exit without adding the fact

                # Add the new room fact
                with open(self.path, 'a') as file:
                    file.write(fact + ".\n")
                
                # Show success dialog
                messagebox.showinfo("Success", f"Room '{room_name}' with capacity {capacity} has been added successfully!")
            except Exception as e:
                # Show error dialog for any other errors
                messagebox.showerror("Error", f"Error adding room: {e}")
        else:
            # Show error dialog for invalid input
            messagebox.showerror("Invalid Input", "Please enter a valid room name and a numeric capacity.")
