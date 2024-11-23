import tkinter as tk
from tkinter import ttk, messagebox
from reactButton import RectButton


class EditNumberOfStudentsPage(tk.Frame):
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent, bg='#DEF2F1')
        self.controller = controller
        self.bgColor = '#DEF2F1'

        self.path = "ScheduleOrganiser/NumberOfStudents.pl"

        # Title label
        self.title_label = tk.Label(
            self,
            text="Edit Number of Students",
            font=("Helvetica", 40, "bold"),
            bg=self.bgColor, fg="#17252A"
        )
        self.title_label.place(relx=0.5, rely=0.1, anchor='center')

        # Year Dropdown Label
        self.year_label = tk.Label(
            self,
            text="Select Year:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.year_label.place(relx=0.2, rely=0.25, anchor='w')

        # Year Dropdown
        self.year_dropdown = ttk.Combobox(
            self,
            state="readonly",
            font=("Helvetica", 14),
            width=25
        )
        self.year_dropdown.place(relx=0.4, rely=0.25, anchor='w')
        self.year_dropdown.bind("<<ComboboxSelected>>", self.populate_capacity)

        # Capacity Label and Entry
        self.capacity_label = tk.Label(
            self,
            text="Capacity:",
            font=("Helvetica", 16),
            bg=self.bgColor, fg="#000000"
        )
        self.capacity_label.place(relx=0.2, rely=0.35, anchor='w')

        self.capacity_entry = tk.Entry(
            self,
            font=("Helvetica", 16),
            width=20
        )
        self.capacity_entry.place(relx=0.4, rely=0.35, anchor='w')

        # Edit Capacity Button
        self.edit_button = RectButton(
            self,
            text="Edit Capacity",
            command=self.edit_capacity,
            width=140,
            height=40,
            bg_color="#FFB400",  # Orange button
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.edit_button.place(relx=0.5, rely=0.45, anchor='center')


        # Back Button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=lambda: controller.show_frame("StudentsFactsPage"),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

        self.load_years()

    def load_years(self):
        """Load all years from the Prolog file."""
        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            years = []
            for line in lines:
                if line.startswith("capacity("):
                    year = line.split('(')[1].split(',')[0].strip()
                    years.append(year)

            self.year_dropdown['values'] = sorted(years)  # Populate dropdown
        except Exception as e:
            messagebox.showerror("Error", f"Error loading years: {e}")

    def populate_capacity(self, event=None):
        """Populate the capacity entry based on the selected year."""
        selected_year = self.year_dropdown.get()

        try:
            with open(self.path, 'r') as file:
                lines = file.readlines()

            for line in lines:
                if line.startswith(f"capacity({selected_year},"):
                    capacity = line.split(',')[1].strip().strip(').')
                    self.capacity_entry.delete(0, tk.END)
                    self.capacity_entry.insert(0, capacity)
                    return

        except Exception as e:
            messagebox.showerror("Error", f"Error loading capacity: {e}")

    def edit_capacity(self):
        """Edit the capacity for the selected year in the Prolog knowledge base."""
        selected_year = self.year_dropdown.get()
        capacity = self.capacity_entry.get().strip()

        if selected_year and capacity.isdigit():
            capacity = int(capacity)
            fact = f"capacity({selected_year}, {capacity})"

            try:
                # Read the file to update the capacity fact
                with open(self.path, 'r') as file:
                    existing_facts = file.readlines()

                updated_facts = []
                fact_found = False
                for line in existing_facts:
                    if line.startswith(f"capacity({selected_year},"):
                        updated_facts.append(fact + ".\n")  # Replace the existing fact
                        fact_found = True
                    else:
                        updated_facts.append(line)

                if not fact_found:
                    messagebox.showerror("Error", f"Year '{selected_year}' does not exist.")
                    return

                # Write the updated facts back to the file
                with open(self.path, 'w') as file:
                    file.writelines(updated_facts)

                messagebox.showinfo("Success", f"Capacity for Year '{selected_year}' has been updated.")
            except Exception as e:
                messagebox.showerror("Error", f"Error editing capacity: {e}")
        else:
            messagebox.showerror("Invalid Input", "Please select a year and enter a valid capacity.")
