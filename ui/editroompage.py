import tkinter as tk
from reactButton import RectButton
from tkinter import messagebox


class EditRoomPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'

        self.path = "ScheduleOrganiser/Rooms.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Edit Room",
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

        # Edit Room Button
        self.edit_button = RectButton(
            self,
            text="Edit Room",
            command=self.edit_room,
            width=140,
            height=40,
            bg_color="#FFB400",  # Orange button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.edit_button.place(relx=0.5, rely=0.55, anchor='center')

        # Delete Room Button
        self.delete_button = RectButton(
            self,
            text="Delete Room",
            command=self.delete_room,
            width=140,
            height=40,
            bg_color="#B22222",  # Red button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.delete_button.place(relx=0.5, rely=0.65, anchor='center')

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

    def edit_room(self):
        """Edit the room in the Prolog knowledge base."""
        room_name = self.room_name_entry.get().strip()
        capacity = self.capacity_entry.get().strip()

        if room_name and capacity.isdigit():
            capacity = int(capacity)
            fact = f"room('{room_name}', {capacity})"

            try:
                # Read the file to update the room fact
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                # Extract the existing room fact to replace
                updated_facts = []
                fact_found = False
                for line in existing_facts:
                    if line.startswith(f"room('{room_name}',"):
                        updated_facts.append(fact + ".\n")  # Replace the existing fact
                        fact_found = True
                    else:
                        updated_facts.append(line)

                if not fact_found:
                    messagebox.showerror("Error", f"Room '{room_name}' does not exist.")
                    return  # Exit without updating

                # Write the updated facts back to the file
                with open(self.path, 'w') as file:
                    file.writelines(updated_facts)

                # Show success dialog
                messagebox.showinfo("Success", f"Room '{room_name}' has been updated to capacity {capacity}.")
            except Exception as e:
                # Show error dialog for any other errors
                messagebox.showerror("Error", f"Error editing room: {e}")
        else:
            # Show error dialog for invalid input
            messagebox.showerror("Invalid Input", "Please enter a valid room name and a numeric capacity.")

    def delete_room(self):
        """Delete the room from the Prolog knowledge base."""
        room_name = self.room_name_entry.get().strip()

        if room_name:
            try:
                # Read the file to find and remove the room fact
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                # Remove the room fact
                updated_facts = [
                    line for line in existing_facts if not line.startswith(f"room('{room_name}',")
                ]

                if len(updated_facts) == len(existing_facts):
                    # Room not found
                    messagebox.showerror("Error", f"Room '{room_name}' does not exist.")
                    return  # Exit without deleting

                # Write the updated facts back to the file
                with open(self.path, 'w') as file:
                    file.writelines(updated_facts)

                # Show success dialog
                messagebox.showinfo("Success", f"Room '{room_name}' has been deleted.")
            except Exception as e:
                # Show error dialog for any other errors
                messagebox.showerror("Error", f"Error deleting room: {e}")
        else:
            # Show error dialog for invalid input
            messagebox.showerror("Invalid Input", "Please enter a valid room name.")
