import tkinter as tk
from tkinter import ttk, messagebox
from reactButton import RectButton


class EditLecturerPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'

        self.path = "ScheduleOrganiser/Lecturer.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Edit Lecturer",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.1, anchor='center')

        # Lecturer Dropdown Label
        self.lecturer_label = tk.Label(
            self,
            text="Select Lecturer:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.lecturer_label.place(relx=0.2, rely=0.25, anchor='w')

        # Lecturer Dropdown
        self.lecturer_dropdown = ttk.Combobox(
            self,
            state="readonly",
            font=("Helvetica", 14),
            width=25
        )
        self.lecturer_dropdown.place(relx=0.4, rely=0.25, anchor='w')
        self.lecturer_dropdown.bind("<<ComboboxSelected>>", self.populate_subjects)

        # Subject Dropdown Label
        self.subject_label = tk.Label(
            self,
            text="Select Subject:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.subject_label.place(relx=0.2, rely=0.35, anchor='w')

        # Subject Dropdown
        self.subject_dropdown = ttk.Combobox(
            self,
            state="readonly",
            font=("Helvetica", 14),
            width=25
        )
        self.subject_dropdown.place(relx=0.4, rely=0.35, anchor='w')

        # Year Label and Entry
        self.year_label = tk.Label(
            self,
            text="Year:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.year_label.place(relx=0.2, rely=0.45, anchor='w')

        self.year_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.year_entry.place(relx=0.4, rely=0.45, anchor='w')

        # Edit Lecturer Button
        self.edit_button = RectButton(
            self,
            text="Edit Lecturer",
            command=self.edit_lecturer,
            width=140,
            height=40,
            bg_color="#FFB400",  # Orange button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.edit_button.place(relx=0.5, rely=0.55, anchor='center')

        # Delete Subject Button
        self.delete_subject_button = RectButton(
            self,
            text="Delete Subject",
            command=self.delete_subject,
            width=140,
            height=40,
            bg_color="#B22222",  # Red button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.delete_subject_button.place(relx=0.4, rely=0.7, anchor='center')

        # Delete Lecturer Button
        self.delete_lecturer_button = RectButton(
            self,
            text="Delete Lecturer",
            command=self.delete_lecturer,
            width=140,
            height=40,
            bg_color="#8B0000",  # Dark Red button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.delete_lecturer_button.place(relx=0.6, rely=0.7, anchor='center')

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

        self.load_lecturers()

    def load_lecturers(self):
        """Load all lecturers from the Prolog file."""
        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            lecturers = set()  # Avoid duplicates
            for line in lines:
                if line.startswith("lecturer("):
                    lecturer_name = line.split('(')[1].split(',')[0].strip("'")
                    lecturers.add(lecturer_name)

            self.lecturer_dropdown['values'] = sorted(list(lecturers))  # Populate dropdown
        except Exception as e:
            messagebox.showerror("Error", f"Error loading lecturers: {e}")

    def populate_subjects(self, event=None):
        """Populate the subjects dropdown based on the selected lecturer."""
        selected_lecturer = self.lecturer_dropdown.get()

        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            subjects = []
            for line in lines:
                if line.startswith(f"lecturer('{selected_lecturer}',"):
                    subject = line.split(',')[1].strip().strip("'")
                    subjects.append(subject)

            self.subject_dropdown['values'] = sorted(subjects)
            if subjects:
                self.subject_dropdown.current(0)  # Automatically select the first subject
        except Exception as e:
            messagebox.showerror("Error", f"Error loading subjects: {e}")

    def edit_lecturer(self):
        """Edit the lecturer's details in the Prolog knowledge base."""
        selected_lecturer = self.lecturer_dropdown.get()
        selected_subject = self.subject_dropdown.get()
        year = self.year_entry.get().strip()

        if selected_lecturer and selected_subject and year.isdigit():
            year = int(year)
            fact = f"lecturer('{selected_lecturer}', '{selected_subject}', {year})"

            try:
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                updated_facts = []
                fact_found = False
                for line in existing_facts:
                    if line.startswith(f"lecturer('{selected_lecturer}', '{selected_subject}',"):
                        updated_facts.append(fact + ".\n")  # Replace the existing fact
                        fact_found = True
                    else:
                        updated_facts.append(line)

                if not fact_found:
                    messagebox.showerror("Error", f"Lecturer '{selected_lecturer}' with subject '{selected_subject}' does not exist.")
                    return

                with open(self.path, 'w') as file:
                    file.writelines(updated_facts)

                messagebox.showinfo("Success", f"Lecturer '{selected_lecturer}' has been updated.")
            except Exception as e:
                messagebox.showerror("Error", f"Error editing lecturer: {e}")
        else:
            messagebox.showerror("Invalid Input", "Please select a lecturer, a subject, and enter a valid year.")

    def delete_subject(self):
        """Delete the selected subject for the selected lecturer."""
        selected_lecturer = self.lecturer_dropdown.get()
        selected_subject = self.subject_dropdown.get()

        if selected_lecturer and selected_subject:
            try:
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                updated_facts = [
                    line for line in existing_facts
                    if not line.startswith(f"lecturer('{selected_lecturer}', '{selected_subject}',")
                ]

                if len(updated_facts) == len(existing_facts):
                    messagebox.showerror("Error", f"Subject '{selected_subject}' for Lecturer '{selected_lecturer}' does not exist.")
                    return

                with open(self.path, 'w') as file:
                    file.writelines(updated_facts)

                messagebox.showinfo("Success", f"Subject '{selected_subject}' for Lecturer '{selected_lecturer}' has been deleted.")
                self.populate_subjects()
            except Exception as e:
                messagebox.showerror("Error", f"Error deleting subject: {e}")
        else:
            messagebox.showerror("Invalid Input", "Please select a lecturer and a subject.")

    def delete_lecturer(self):
        """Delete all subjects for the selected lecturer."""
        selected_lecturer = self.lecturer_dropdown.get()

        if selected_lecturer:
            try:
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                updated_facts = [
                    line for line in existing_facts if not line.startswith(f"lecturer('{selected_lecturer}',")
                ]

                if len(updated_facts) == len(existing_facts):
                    messagebox.showerror("Error", f"Lecturer '{selected_lecturer}' does not exist.")
                    return

                with open(self.path, 'w') as file:
                    file.writelines(updated_facts)

                messagebox.showinfo("Success", f"Lecturer '{selected_lecturer}' has been deleted.")
                self.load_lecturers()
                self.subject_dropdown.set('')
            except Exception as e:
                messagebox.showerror("Error", f"Error deleting lecturer: {e}")
        else:
            messagebox.showerror("Invalid Input", "Please select a lecturer.")
