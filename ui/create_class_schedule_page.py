import tkinter as tk
from tkinter import ttk
from reactButton import RectButton

class CreateClassSchedulePage(tk.Frame):

    def __init__(self, parent, controller):
        self.bgColor = '#DEF2F1'  # Background color for the frame
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller
        self.subjects_list = []  # List to store input data

        # Configure the grid to make it responsive (only if necessary for other widgets)
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # Set the dimensions of the frame (you can adjust these as needed)
        self.place(relwidth=1, relheight=1)

        # Create a container frame for form content (70% width and 60% height of window)
        self.container = tk.Frame(self, bg=self.bgColor)
        self.container.place(relx=0.5, rely=0.54, relwidth=0.69, relheight=0.7, anchor='center')

        # Back button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=self.on_back_click,
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.back_button.place(x=20, y=20)

        # Initialize the form
        self.init()

    def on_back_click(self):
        self.controller.show_frame('HomePage')

    def add_entry(self):
        # Get the input data
        subject = self.subject_entry.get()
        professor = self.professor_entry.get() if not self.skip_professor_var.get() else "Not Specified"
        duration = self.duration_entry.get()

        # Validate that both fields are filled
        if subject and duration and (professor or self.skip_professor_var.get()):
            # Add the entry to the subjects_list
            self.subjects_list.append({
                "subject": subject,
                "professor": professor,
                "duration": duration + " hours"
            })

            # Clear the input fields after adding
            self.subject_entry.delete(0, tk.END)
            self.professor_entry.delete(0, tk.END)
            self.duration_entry.delete(0, tk.END)
            self.skip_professor_var.set(False)  # Reset the checkbox

            # Display the current entries to the user
            self.update_entry_display()
        else:
            if not professor:
                self.professor_entry.config(highlightcolor="red", highlightthickness=2)
            print("Subject, Duration, and Professor are required!")

    def update_entry_display(self):
        # Clear the previous display
        for widget in self.scrollable_frame.winfo_children():
            widget.destroy()
        
        # Display each entry with labels that can be clicked to remove them
        for idx, entry in enumerate(self.subjects_list):
            entry_label = tk.Label(
                self.scrollable_frame,
                text=f"{idx + 1}. Subject: {entry['subject']} | Professor: {entry['professor']} | Duration: {entry['duration']}",
                font=("Poppins", 12),
                bg='#FEFFFF',
                fg='#17252A',
                anchor='w'
            )
            entry_label.pack(fill='x', pady=5, padx=10)

            # Bind the click event to remove the entry
            entry_label.bind("<Button-1>", lambda e, i=idx: self.remove_entry(i))

            # Bind hover effect
            entry_label.bind("<Enter>", lambda e, label=entry_label: label.config(fg='red', underline=1))
            entry_label.bind("<Leave>", lambda e, label=entry_label: label.config(fg='#17252A', underline=0))


    def remove_entry(self, index):
        # Remove the entry at the specified index
        if 0 <= index < len(self.subjects_list):
            del self.subjects_list[index]
            self.update_entry_display()  # Refresh the display after removing an entry

    def validate_add_button_state(self, *args):
        # Enable or disable the "Add" button based on form inputs
        if self.subject_entry.get() and self.duration_entry.get() and (
                self.skip_professor_var.get() or self.professor_entry.get()):
            self.add_button.config(state='normal')
        else:
            self.add_button.config(state='disabled')

    def submit_entries(self):
        # Finalize the entries
        print("Submitted Entries:", self.subjects_list)
        self.controller.show_frame('ClassSchedulePage')
        # Here you can process the data further, save it, or use it in other parts of your application
        
    def init(self):
        # Form frame to contain the input fields and buttons
        form_frame = tk.Frame(self.container, bg='#FFF', bd=2, relief='solid')
        form_frame.place(relx=0.5, rely=0.5, relwidth=1, relheight=0.85, anchor='center')

        # Header Label
        self.header_label = tk.Label(
            self,
            text="Create Class Schedule",
            font=("Poppins", 30, "bold"),
            bg=self["bg"],
            fg="#17252A"
        )
        self.header_label.place(relx=0.5, rely=0.2, anchor="center")

        # Subject Entry Label and Field
        subject_label = tk.Label(form_frame, text="Subject:", font=("Poppins", 14), fg="#17252A", bg='#FFF')
        subject_label.place(relx=0.1, rely=0.05)
        self.subject_entry = tk.Entry(form_frame, font=("Poppins", 12), relief='solid')
        self.subject_entry.place(relx=0.3, rely=0.05, relwidth=0.6, height=40)

        # Professor Entry Label and Field
        professor_label = tk.Label(form_frame, text="Professor:", font=("Poppins", 14), fg="#17252A", bg='#FFF')
        professor_label.place(relx=0.1, rely=0.20)
        self.professor_entry = tk.Entry(form_frame, font=("Poppins", 12), relief='solid')
        self.professor_entry.place(relx=0.3, rely=0.20, relwidth=0.6, height=40)

        # Checkbox to skip professor entry
        self.skip_professor_var = tk.BooleanVar()
        skip_professor_check = tk.Checkbutton(
            form_frame,
            text="Not Specify Professor",
            font=("Poppins", 12),
            variable=self.skip_professor_var,
            bg='#FFF',
            fg="#17252A",
            command=self.validate_add_button_state  # Recheck button state when checkbox changes
        )
        skip_professor_check.place(relx=0.3, rely=0.3)

        # Duration Entry Label and Field
        duration_label = tk.Label(form_frame, text="Duration (hours):", font=("Poppins", 14), fg="#17252A", bg='#FFF')
        duration_label.place(relx=0.1, rely=0.4)
        self.duration_entry = tk.Entry(form_frame, font=("Poppins", 12), relief='solid')
        self.duration_entry.place(relx=0.3, rely=0.4, relwidth=0.6, height=40)

        # Add button
        self.add_button = RectButton(
            form_frame,
            text="Add",
            command=self.add_entry,
            width=100,
            height=30,
            bg_color="#3AAFA9",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        self.add_button.place(relx=0.3, rely=0.55)
        self.add_button.config(state='disabled')  # Initially disable the button

        # Submit button
        submit_button = RectButton(
            form_frame,
            text="Submit",
            command=self.submit_entries,
            width=100,
            height=30,
            bg_color="#3AAFA9",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold")
        )
        submit_button.place(relx=0.5, rely=0.55)

        # Scrollable display area for current entries
        scrollable_frame_container = tk.Frame(form_frame, bg='#FEFFFF')
        scrollable_frame_container.place(relx=0.1, rely=0.65, relwidth=0.8, relheight=0.3)  # Adjusted height and margin

        # Create a canvas and a scrollbar for the scrollable area
        self.canvas = tk.Canvas(scrollable_frame_container, bg='#FEFFFF')
        scrollbar = ttk.Scrollbar(scrollable_frame_container, orient='vertical', command=self.canvas.yview)
        self.scrollable_frame_content = tk.Frame(self.canvas, bg='#FEFFFF')

        # Configure the scrollbar
        self.scrollable_frame_content.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )
        self.canvas.create_window((0, 0), window=self.scrollable_frame_content, anchor="nw")
        self.canvas.configure(yscrollcommand=scrollbar.set)

        # Pack canvas and scrollbar
        self.canvas.pack(side="left", fill="both", expand=True, pady=(0, 20))  # Added bottom margin with `pady`
        scrollbar.pack(side="right", fill="y")

        # Frame to hold the scrollable content
        self.scrollable_frame = tk.Frame(self.scrollable_frame_content, bg='#FEFFFF')
        self.scrollable_frame.pack(fill='both', expand=True)

        # Bind validation after user input
        self.subject_entry.bind("<KeyRelease>", self.validate_add_button_state)
        self.professor_entry.bind("<KeyRelease>", self.validate_add_button_state)
        self.duration_entry.bind("<KeyRelease>", self.validate_add_button_state)
