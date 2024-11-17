import tkinter as tk
from components.roundedButton import RoundButton  # Assuming this is a custom class you defined
from components.rectButton import RectButton

def get_user_info():
    user_info = {}
    try:
        with open('user_profile.txt') as file:
            lines = file.readlines()
            for _, line in enumerate(lines):
                info = line.split('=')
                info[1] = info[1].strip()
                # if dont have info assign None 
                if info[1] == "''" or info[1] == '':
                    user_info[info[0]] = None
                else:
                    user_info[info[0]] = info[1]
    except Exception as e:
        print(e)
    return user_info


class ProfilePage(tk.Frame):
    def __init__(self, parent, controller):
        self.get_user_info()
        self.bgColor = '#DEF2F1'  # Background color for the frame
        tk.Frame.__init__(self, parent, bg=self.bgColor)
        self.controller = controller

        # Configure the grid to make it responsive (only if necessary for other widgets)
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # Set the dimensions of the frame (you can adjust these as needed)
        self.place(relwidth=1, relheight=1)

        # Create a container frame for form content (70% width and 60% height of window)
        container = tk.Frame(self, bg=self.bgColor)  # Make the background transparent here
        container.place(relx=0.5, rely=0.5, relwidth=0.7, relheight=0.6, anchor='center')

        # Create a frame for the Back button at the top-left corner with no background
        self.back_button = RoundButton(
            self, text="< BACK",
            command=self.on_back_click,
            bg='#000', fg='#FFF', font=('Helvetica', 14, 'bold')  # Keep the button's background
        )
        self.back_button.place(x=10, y=10)  # Top-left corner with a little padding

        # Create a frame for the form to centralize it within the container
        form_frame = tk.Frame(container, bg='#FFF', bd=2, relief='solid')
        form_frame.place(relx=0.5, rely=0.5, relwidth=1, relheight=0.4, anchor='center')  # Center the form frame

        # Form title with underline
        title_label = tk.Label(
            form_frame, text="Personal Information",
            font=('Helvetica', 28, 'bold underline'),  # Underlined font
            anchor='w', bg='#FFF'
        )
        title_label.place(x=20, y=20)

        # First row: Name and Email with placeholder-like behavior
        self.create_entry_row(form_frame, "Name", "Enter your email", "Name", "Email", 1)

        # Second row: Faculty and Year with placeholder-like behavior
        self.create_entry_row(form_frame, "Faculty", "Enter your year", "Faculty", "Year", 2)

    def create_entry_row(self, parent, left_placeholder, right_placeholder, left_field, right_field, row):
        # Function to handle placeholder behavior
        def add_placeholder(entry, placeholder_text):
            entry.insert(0, placeholder_text)
            entry.config(fg='gray')

        def remove_placeholder(event, entry, placeholder_text):
            if entry.get() == placeholder_text:
                entry.delete(0, 'end')
                entry.config(fg='black')

        def restore_placeholder(event, entry, placeholder_text):
            if not entry.get():
                add_placeholder(entry, placeholder_text)

        # Left entry with placeholder behavior
        left_entry = tk.Entry(
            parent,
            font=('Helvetica', 16),
            highlightthickness=0,  # Removes the focus ring
            bd=1,  # Keeps the border
            relief='solid'
        )

        # Check if user info exists for the left field and update the placeholder accordingly
        if self.user_info.get(left_field.lower()) is not None:
            left_entry.insert(0, self.user_info.get(left_field.lower()))  # Use stored value
            left_entry.config(fg='black')
        else:
            add_placeholder(left_entry, left_placeholder)  # Use placeholder if no value exists

        left_entry.bind("<FocusIn>", lambda e: remove_placeholder(e, left_entry, left_placeholder))
        left_entry.bind("<FocusOut>", lambda e: restore_placeholder(e, left_entry, left_placeholder))
        left_entry.bind("<KeyRelease>", lambda e: self.on_input_change(left_entry, left_field))  # Pass specific field name
        left_entry.place(x=20, y=60 + (row - 1) * 60, width=250, height=40)  # Adjust position and size

        # Right entry with placeholder behavior
        right_entry = tk.Entry(
            parent,
            font=('Helvetica', 16),
            highlightthickness=0,  # Removes the focus ring
            bd=1,  # Keeps the border
            relief='solid'
        )

        # Check if user info exists for the right field and update the placeholder accordingly
        if self.user_info.get(right_field.lower()) is not None:
            right_entry.insert(0, self.user_info.get(right_field.lower()))  # Use stored value
            right_entry.config(fg='black')
        else:
            add_placeholder(right_entry, right_placeholder)  # Use placeholder if no value exists

        right_entry.bind("<FocusIn>", lambda e: remove_placeholder(e, right_entry, right_placeholder))
        right_entry.bind("<FocusOut>", lambda e: restore_placeholder(e, right_entry, right_placeholder))
        right_entry.bind("<KeyRelease>", lambda e: self.on_input_change(right_entry, right_field))  # Pass specific field name
        right_entry.place(x=290, y=60 + (row - 1) * 60, width=250, height=40)  # Adjust position and size

    def get_user_info(self):
        # read user info from 'user_profile.txt'
        self.user_info = get_user_info()

    def on_input_change(self, entry, field):
        # Action to perform every time an input changes
        field = field.lower()
        self.user_info[field] = entry.get()

    def on_back_click(self):
        # Logic to handle the back button click
        self.controller.show_frame('HomePage')  # Assuming this method exists in your controller

        # Save user info to file 'user_profile.txt'
        try:
            with open('user_profile.txt', 'w') as file:
                # save user info if it contains something else save info as ''
                name = self.user_info['name'] if self.user_info['name'] is not None and self.user_info['name'] != '' else "''"
                email = self.user_info['email'] if self.user_info['email'] is not None and self.user_info['email'] != '' else "''"
                faculty = self.user_info['faculty'] if self.user_info['faculty'] is not None and self.user_info['faculty'] != '' else "''"
                year = self.user_info['year'] if self.user_info['year'] is not None and self.user_info['year'] != '' else "''"
                
                content = f"name={name}\nemail={email}\nfaculty={faculty}\nyear={year}"
                file.write(content)
        except Exception as e:
            print(e)
