import tkinter as tk
from reactButton import RectButton
from tkinter import messagebox

class AddLecturerPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'
        
        self.path = "ScheduleOrganiser/Lecturer.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Add Lecturer",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.1, anchor='center')

        # Lecturer Name Label and Entry
        self.lecturer_name_label = tk.Label(
            self,
            text="Lecturer Name:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.lecturer_name_label.place(relx=0.2, rely=0.3, anchor='w')

        self.lecturer_name_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.lecturer_name_entry.place(relx=0.4, rely=0.3, anchor='w')

        # Subject Label and Entry
        self.subject_label = tk.Label(
            self,
            text="Subject:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.subject_label.place(relx=0.2, rely=0.4, anchor='w')

        self.subject_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.subject_entry.place(relx=0.4, rely=0.4, anchor='w')

        # Year Label and Entry
        self.year_label = tk.Label(
            self,
            text="Year:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.year_label.place(relx=0.2, rely=0.5, anchor='w')

        self.year_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.year_entry.place(relx=0.4, rely=0.5, anchor='w')

        # Add Lecturer Button
        self.add_button = RectButton(
            self,
            text="Add Lecturer",
            command=self.add_lecturer,
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
            command=lambda: controller.show_frame("LecturersFactsPage"),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

    def add_lecturer(self):
        """Add the lecturer to the Prolog knowledge base."""
        lecturer_name = self.lecturer_name_entry.get().strip()
        subject = self.subject_entry.get().strip()
        year = self.year_entry.get().strip()

        if lecturer_name and subject and year.isdigit():
            year = int(year)
            fact = f"lecturer('{lecturer_name}', '{subject}', {year})"

            try:
                # Read the file to check if the lecturer already exists
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                # Extract lecturer names from existing facts and check for duplicates
                existing_lecturer_names = [
                    line.split('(')[1].split(',')[0].strip("'")  # Extract lecturer name part
                    for line in existing_facts
                    if line.startswith("lecturer(")  # Ensure it's a lecturer fact
                ]

                if lecturer_name in existing_lecturer_names:
                    # Show error dialog for duplicate lecturer name
                    messagebox.showerror("Duplicate Entry", f"Lecturer '{lecturer_name}' already exists.")
                    return  # Exit without adding the fact

                # Add the new lecturer fact
                with open(self.path, 'a') as file:
                    file.write(fact + ".\n")
                
                # Show success dialog
                messagebox.showinfo("Success", f"Lecturer '{lecturer_name}' with subject '{subject}' for year {year} has been added successfully!")
            except Exception as e:
                # Show error dialog for any other errors
                messagebox.showerror("Error", f"Error adding lecturer: {e}")
        else:
            # Show error dialog for invalid input
            messagebox.showerror("Invalid Input", "Please enter valid lecturer details.")
