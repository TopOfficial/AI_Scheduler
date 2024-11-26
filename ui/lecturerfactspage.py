from pyswip import Prolog
import tkinter as tk
from tkinter import ttk, messagebox
from tkinter import Canvas, Scrollbar
from reactButton import RectButton


class LecturersFactsPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.prolog = Prolog()  # Initialize Prolog engine
        self.prolog.consult("ScheduleOrganiser/Lecturer.pl")  # Load Lecturer.pl file
        self.bgColor = '#DEF2F1'
        
        self.path = "ScheduleOrganiser/Lecturer.pl"

        # Back button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=self.on_back_click,
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

        # Add button for adding new facts
        self.add_button = RectButton(
            self,
            text="Add Lecturer",
            command=self.on_add_click,
            width=140,
            height=40,
            bg_color="#0F6004",  # Green button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.add_button.place(relx=0.7, rely=0.9, anchor='center')  # Center the button below the scroll frame

        # Edit button
        self.edit_button = RectButton(
            self,
            text="Edit",
            command=self.on_edit_click,
            width=140,
            height=40,
            bg_color="#FFB400",  # Orange button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.edit_button.place(relx=0.3, rely=0.9, anchor='center')  # Align to the left
        
                # Add Subject Button
        self.add_subject_button = RectButton(
            self,
            text="Add Subject",
            command=self.add_subject_popup,
            width=140,
            height=40,
            bg_color="#0066CC",  # Blue button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.add_subject_button.place(relx=0.5, rely=0.9, anchor='center')

        # Add title
        self.title_label = tk.Label(
            self,
            text="Lecturers Facts",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.13, anchor='center')  # Center the title

        # Scrollable canvas for form frame
        self.scroll_canvas = Canvas(self, bg=self.bgColor, bd=2, relief='solid')
        self.scroll_canvas.place(relx=0.5, rely=0.5, relheight=0.6, anchor='center')

        # Add a vertical scrollbar
        self.scrollbar = Scrollbar(self, orient="vertical", command=self.scroll_canvas.yview)
        self.scrollbar.place(relx=0.65, rely=0.5, relheight=0.6, anchor='center')

        # Configure the canvas with the scrollbar
        self.scroll_canvas.configure(yscrollcommand=self.scrollbar.set)

        # Frame inside the canvas to hold dynamic content
        self.form_frame = tk.Frame(self.scroll_canvas, bg=self.bgColor)
        self.canvas_window = self.scroll_canvas.create_window((0, 0), window=self.form_frame, anchor='nw')

        # Bind the canvas to update its scrollregion dynamically
        self.scroll_canvas.bind('<Configure>', self.update_scrollregion)

    def init(self):
        """Initialize or refresh the dynamic content in the form frame."""
        print("Initializing LecturersFactsPage...")
        
        # Query Prolog to get lecturer facts
        lecturer_facts = self.get_lecturer_facts()

        # Clear existing form frame content
        for widget in self.form_frame.winfo_children():
            widget.destroy()

        # Add header row
        headers = ["Lecturer Name", "Subject", "Year"]
        for col, header in enumerate(headers):
            tk.Label(
                self.form_frame,
                text=header,
                font=('Helvetica', 16, 'bold'),
                bg=self.bgColor, fg='#000000'
            ).grid(row=0, column=col, padx=10, pady=10, sticky='w')  # Header row

        # Display lecturer facts in a three-column grid format
        for index, lecturer in enumerate(lecturer_facts, start=1):
            tk.Label(
                self.form_frame,
                text=lecturer['Name'],
                font=('Helvetica', 14),
                bg=self.bgColor, fg='#000000'
            ).grid(row=index, column=0, padx=10, pady=10, sticky='w')  # Name

            tk.Label(
                self.form_frame,
                text=lecturer['Subject'],
                font=('Helvetica', 14),
                bg=self.bgColor, fg='#000000'
            ).grid(row=index, column=1, padx=10, pady=10, sticky='w')  # Subject

            tk.Label(
                self.form_frame,
                text=lecturer['Year'],
                font=('Helvetica', 14),
                bg=self.bgColor, fg='#000000'
            ).grid(row=index, column=2, padx=10, pady=10, sticky='w')  # Year

        # Update the scrollregion to match the new content
        self.update_scrollregion()

    def update_scrollregion(self, event=None):
        """Update the scrollregion of the canvas to match the size of the form_frame."""
        self.scroll_canvas.update_idletasks()
        self.scroll_canvas.config(scrollregion=self.scroll_canvas.bbox("all"))

    def get_lecturer_facts(self):
        """Fetch lecturer facts from Prolog."""
        print("Querying Prolog for lecturer facts...")
        lecturers = list(self.prolog.query("lecturer(Name, Subject, Year)."))
        print("Fetched lecturer facts:", lecturers)
        return lecturers

    def on_add_click(self):
        """Handle the Add Lecturer button click."""
        print("Add Lecturer button clicked")
        # Example action: Navigate to an AddLecturerPage (you need to implement AddLecturerPage)
        self.controller.show_frame("AddLecturerPage")

    def on_edit_click(self):
        """Handle the Edit Lecturer button click."""
        print(f"Edit Lecturer button clicked")
        # Example action: Navigate to an EditLecturerPage (you need to implement EditLecturerPage)
        self.controller.show_frame("EditLecturerPage")

    def on_back_click(self):
        """Navigate back to the AddFactsPage."""
        self.controller.show_frame("AddFactsPage")


    def add_subject_popup(self):
        """Show a popup to add a subject to an existing lecturer."""
        popup = tk.Toplevel(self)
        popup.title("Add Subject")
        popup.geometry("400x300")
        popup.configure(bg=self.bgColor)

        # Lecturer Dropdown
        tk.Label(popup, text="Select Lecturer:", font=("Helvetica", 14), bg=self.bgColor, fg="#000000").pack(pady=10)
        lecturer_dropdown = ttk.Combobox(popup, state="readonly", font=("Helvetica", 14), width=30)
        lecturer_dropdown.pack(pady=5)
        self.load_lecturers(lecturer_dropdown)

        # Subject Name Entry
        tk.Label(popup, text="Subject Name:", font=("Helvetica", 14), bg=self.bgColor, fg="#000000").pack(pady=10)
        subject_entry = tk.Entry(popup, font=("Helvetica", 14), width=30)
        subject_entry.pack(pady=5)

        # Year Entry
        tk.Label(popup, text="Subject Year:", font=("Helvetica", 14), bg=self.bgColor, fg="#000000").pack(pady=10)
        year_entry = tk.Entry(popup, font=("Helvetica", 14), width=30)
        year_entry.pack(pady=5)

        # Add Subject Button
        RectButton(
            popup,
            text="Add Subject",
            command=lambda: self.add_subject(lecturer_dropdown.get(), subject_entry.get(), year_entry.get(), popup),
            width=140,
            height=40,
            bg_color="#0F6004",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        ).pack(pady=20)

    def load_lecturers(self, dropdown):
        """Load lecturers into the dropdown."""
        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            lecturers = set(
                line.split('(')[1].split(',')[0].strip("'") for line in lines if line.startswith("lecturer(")
            )
            dropdown['values'] = sorted(lecturers)
            if lecturers:
                dropdown.current(0)
        except Exception as e:
            messagebox.showerror("Error", f"Error loading lecturers: {e}")

    def add_subject(self, lecturer, subject, year, popup):
        """Add a subject to an existing lecturer."""
        if lecturer and subject and year.isdigit():
            year = int(year)
            fact = f"lecturer('{lecturer}', '{subject}', {year})"

            try:
                # Read the file to check for duplicates
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                # Check for duplicate lecturer and subject for the same year
                if any(fact.strip() + "." == line.strip() for line in existing_facts):
                    messagebox.showerror("Duplicate Entry", "This subject already exists for the selected lecturer and year.")
                    return

                # Add the new subject fact
                with open(self.path, 'a') as file:
                    file.write(fact + ".\n")

                # Show success dialog and close popup
                messagebox.showinfo("Success", "Subject added successfully!")
                popup.destroy()
            except Exception as e:
                messagebox.showerror("Error", f"Error adding subject: {e}")
        else:
            messagebox.showerror("Invalid Input", "Please enter all fields with valid values.")
