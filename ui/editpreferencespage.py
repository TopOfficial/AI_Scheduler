import tkinter as tk
from tkinter import ttk, messagebox
from reactButton import RectButton


class EditPreferencesPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'
        self.path = "ScheduleOrganiser/Preferences.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Edit Preference",
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
        self.lecturer_label.place(relx=0.2, rely=0.2, anchor='w')

        # Lecturer Dropdown
        self.lecturer_dropdown = ttk.Combobox(self, state="readonly", font=("Helvetica", 14), width=25)
        self.lecturer_dropdown.place(relx=0.4, rely=0.2, anchor='w')
        self.lecturer_dropdown.bind("<<ComboboxSelected>>", self.populate_constraints)
        

        # Constraint Type Dropdown Label
        self.constraint_label = tk.Label(
            self,
            text="Constraint Type:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.constraint_label.place(relx=0.2, rely=0.3, anchor='w')

        # Constraint Type Dropdown
        self.constraint_dropdown = ttk.Combobox(
            self, state="readonly", values=["time", "day", "room"], font=("Helvetica", 14), width=25
        )
        self.constraint_dropdown.place(relx=0.4, rely=0.3, anchor='w')
        self.constraint_dropdown.bind("<<ComboboxSelected>>", self.populate_old_values)

        # Old Constraint Value Dropdown Label
        self.old_value_label = tk.Label(
            self,
            text="Old Constraint Value:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.old_value_label.place(relx=0.2, rely=0.4, anchor='w')

        # Old Constraint Value Dropdown
        self.old_value_dropdown = ttk.Combobox(self, state="readonly", font=("Helvetica", 14), width=25)
        self.old_value_dropdown.place(relx=0.4, rely=0.4, anchor='w')
        self.old_value_dropdown.bind("<<ComboboxSelected>>", self.populate_old_score)

        # Old Score Dropdown Label
        self.old_score_label = tk.Label(
            self,
            text="Old Score:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.old_score_label.place(relx=0.2, rely=0.5, anchor='w')

        # Old Score Dropdown
        self.old_score_dropdown = ttk.Combobox(self, state="readonly", font=("Helvetica", 14), width=25)
        self.old_score_dropdown.place(relx=0.4, rely=0.5, anchor='w')

        # # New Constraint Value Dropdown Label
        # self.new_value_label = tk.Label(
        #     self,
        #     text="New Constraint Value:",
        #     font=("Helvetica", 16),
        #     bg=self.bgColor, fg="#000000"
        # )
        # self.new_value_label.place(relx=0.2, rely=0.6, anchor='w')

        # # New Constraint Value Dropdown
        # self.new_value_dropdown = ttk.Combobox(self, state="readonly", font=("Helvetica", 14), width=25)
        # self.new_value_dropdown.place(relx=0.4, rely=0.6, anchor='w')

        # New Score Entry Label
        self.new_score_label = tk.Label(
            self,
            text="New Score:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.new_score_label.place(relx=0.2, rely=0.6, anchor='w')

        # New Score Entry
        self.new_score_entry = tk.Entry(self, font=("Helvetica", 16), width=25)
        self.new_score_entry.place(relx=0.4, rely=0.6, anchor='w')

        # Edit Button
        self.edit_button = RectButton(
            self,
            text="Edit Preference",
            command=self.edit_preference,
            width=140,
            height=40,
            bg_color="#FFB400",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.edit_button.place(relx=0.4, rely=0.7, anchor='center')

        # Delete Button
        self.delete_button = RectButton(
            self,
            text="Delete Preference",
            command=self.delete_preference,
            width=140,
            height=40,
            bg_color="#B22222",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.delete_button.place(relx=0.6, rely=0.7, anchor='center')

        # Back Button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=self.back,
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

        self.load_lecturers()
        
    def back(self):
        self.controller.show_frame("PreferencesFactsPage")
        self.clear_fields()

    def load_lecturers(self):
        """Load lecturers into the dropdown."""
        try:
            with open("ScheduleOrganiser/Preferences.pl", 'r') as file:
                lines = file.readlines()
            lecturers = set(line.split('(')[1].split(',')[0].strip("'") for line in lines if line.startswith("preference("))
            self.lecturer_dropdown['values'] = sorted(lecturers)
        except Exception as e:
            messagebox.showerror("Error", f"Error loading lecturers: {e}")

    def populate_constraints(self, event=None):
        """Populate constraint types dynamically based on the selected lecturer."""
        selected_lecturer = self.lecturer_dropdown.get()
        if selected_lecturer:
            try:
                with open(self.path, 'r') as file:
                    lines = file.readlines()
                # Determine which constraint types exist for the selected lecturer
                available_constraints = set()
                for line in lines:
                    if line.startswith(f"preference('{selected_lecturer}',"):
                        constraint_type = line.split(',')[1].strip()
                        available_constraints.add(constraint_type)
                self.constraint_dropdown['values'] = sorted(available_constraints)
                self.constraint_dropdown.set("")  # Reset the current selection
                self.old_value_dropdown.set("")  # Reset old value
                self.old_score_dropdown.set("")  # Reset old score
                # self.new_value_dropdown.set("")  # Reset new value
            except Exception as e:
                messagebox.showerror("Error", f"Error loading constraints: {e}")
        else:
            self.constraint_dropdown['values'] = []  # Clear if no lecturer selected


    def populate_old_values(self, event=None):
        """Populate old constraint values based on the lecturer and type."""
        selected_lecturer = self.lecturer_dropdown.get()
        selected_constraint = self.constraint_dropdown.get()
        if selected_lecturer and selected_constraint:
            try:
                with open(self.path, 'r') as file:
                    lines = file.readlines()
                old_values = set()
                for line in lines:
                    if line.startswith(f"preference('{selected_lecturer}', {selected_constraint},"):
                        parts = line.split(',')
                        constraint_value = parts[2].strip().strip("'").strip()
                        old_values.add(constraint_value)
                if old_values:
                    self.old_value_dropdown['values'] = sorted(old_values)
                else:
                    self.old_value_dropdown['values'] = []  # Clear dropdown if no values found
                    messagebox.showinfo("Info", f"No old values found for '{selected_lecturer}' and '{selected_constraint}'.")
            except Exception as e:
                messagebox.showerror("Error", f"Error loading old values: {e}")

    def populate_old_score(self, event=None):
        """Populate the old score based on the lecturer and old value."""
        selected_lecturer = self.lecturer_dropdown.get()
        selected_constraint = self.constraint_dropdown.get()
        old_value = self.old_value_dropdown.get()
        if selected_lecturer and selected_constraint and old_value:
            if selected_constraint == "room":
                try:
                    with open(self.path, 'r') as file:
                        lines = file.readlines()
                    old_scores = set(
                        line.split(',')[3].strip().strip(').') for line in lines
                        if line.startswith(f"preference('{selected_lecturer}', {selected_constraint}, '{old_value}',")
                    )
                    self.old_score_dropdown['values'] = sorted(old_scores)
                except Exception as e:
                    messagebox.showerror("Error", f"Error loading old scores: {e}")
            else:
                try:
                    with open(self.path, 'r') as file:
                        lines = file.readlines()
                    old_scores = set(
                        line.split(',')[3].strip().strip(').') for line in lines
                        if line.startswith(f"preference('{selected_lecturer}', {selected_constraint}, {old_value},")
                    )
                    self.old_score_dropdown['values'] = sorted(old_scores)
                except Exception as e:
                    messagebox.showerror("Error", f"Error loading old scores: {e}")

    def edit_preference(self):
        """Edit the selected preference in the Prolog knowledge base."""
        selected_lecturer = self.lecturer_dropdown.get()
        selected_constraint = self.constraint_dropdown.get()
        old_constraint_value = self.old_value_dropdown.get()
        # new_constraint_value = self.new_value_dropdown.get()
        new_score = self.new_score_entry.get().strip()

        if not (selected_lecturer and selected_constraint and old_constraint_value):
            messagebox.showerror("Invalid Input", "Please select all required fields.")
            return

        if not (new_score.isdigit()):
            messagebox.showerror("Invalid Input", "Please enter a valid new constraint value and numeric score.")
            return

        new_score = int(new_score)
        new_fact = f"preference('{selected_lecturer}', {selected_constraint}, '{old_constraint_value}', {new_score})"

        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            updated_facts = []
            fact_found = False
            for line in lines:
                if line.startswith(f"preference('{selected_lecturer}', {selected_constraint}, '{old_constraint_value}',"):
                    updated_facts.append(new_fact + ".\n")
                    fact_found = True
                else:
                    updated_facts.append(line)

            if not fact_found:
                messagebox.showerror("Error", "The selected preference could not be found.")
                return

            with open(self.path, 'w') as file:
                file.writelines(updated_facts)

            messagebox.showinfo("Success", "Preference updated successfully!")
            self.clear_fields()

        except Exception as e:
            messagebox.showerror("Error", f"Error updating preference: {e}")

    def delete_preference(self):
        """Delete the selected preference in the Prolog knowledge base."""
        selected_lecturer = self.lecturer_dropdown.get()
        selected_constraint = self.constraint_dropdown.get()
        selected_value = self.old_value_dropdown.get()

        if not (selected_lecturer and selected_constraint and selected_value):
            messagebox.showerror("Invalid Input", "Please select all required fields to delete.")
            return

        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            updated_facts = [
                line for line in lines if not line.startswith(f"preference('{selected_lecturer}', {selected_constraint}, '{selected_value}',")
            ]

            if len(updated_facts) == len(lines):
                messagebox.showerror("Error", "The selected preference could not be found.")
                return

            with open(self.path, 'w') as file:
                file.writelines(updated_facts)

            messagebox.showinfo("Success", "Preference deleted successfully!")
            self.clear_fields()

        except Exception as e:
            messagebox.showerror("Error", f"Error deleting preference: {e}")

    def clear_fields(self):
        """Clear all input fields and reload data."""
        self.lecturer_dropdown.set("")
        self.constraint_dropdown.set("")
        self.old_value_dropdown.set("")
        self.old_score_dropdown.set("")
        self.new_score_entry.delete(0, tk.END)
        self.load_lecturers()
