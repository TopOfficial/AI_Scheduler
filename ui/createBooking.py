import tkinter as tk
from pathlib import Path


class CreateBooking(tk.Frame):
    def __init__(self, parent, controller=None):
        super().__init__(parent)
        self.controller = controller
        self.configure(bg="#e8f7f8")  # Light blue background

        # Asset paths (replace with actual asset path if needed)
        self.OUTPUT_PATH = Path(__file__).parent
        self.ASSETS_PATH = self.OUTPUT_PATH / Path(r"/Users/premecwy/Desktop/build/assets/frame0")

        # Canvas for design elements
        self.canvas = tk.Canvas(
            self,
            bg="#e8f7f8",
            height=1024,
            width=1440,
            bd=0,
            highlightthickness=0,
            relief="ridge"
        )
        self.canvas.place(x=0, y=0)

        # Add rectangles for design
        self.canvas.create_rectangle(
            59.0, 65.0, 235.0, 140.0,
            fill="#000000", outline=""
        )
        self.canvas.create_rectangle(
            99.0, 217.0, 1341.0, 720.0,
            fill="#000000", outline=""
        )
        self.canvas.create_rectangle(
            538.0, 812.0, 753.0, 887.0,
            fill="#000000", outline=""
        )

        # Back button
        self.back_button = tk.Button(
            self, text="← BACK", font=("Poppins", 12, "bold"), bg="#000", fg="#fff", borderwidth=0, padx=10,
            command=self.go_back
        )
        self.back_button.place(x=20, y=20)

        # Header
        self.header_label = tk.Label(
            self, text="Create Booking", font=("Poppins", 16, "bold"), bg="#e8f7f8", anchor="w"
        )
        self.header_label.place(x=30, y=80)

        # Input form frame
        self.form_frame = tk.Frame(self, bg="#fff", padx=10, pady=10, borderwidth=1, relief="solid")
        self.form_frame.place(x=100, y=230, width=1140, height=400)

        # Form fields
        self.fields = [
            ("Lab number", "Date"),
            ("Start time", "End time"),
            ("Faculty", "Year"),
            ("Number of participants", "Topic"),
        ]
        self.entry_widgets = []  # Store references to entry widgets for later use

        self.create_form()

        # Create button
        self.create_button = tk.Button(
            self, text="CREATE", font=("Poppins", 12, "bold"), bg="#000", fg="#fff", padx=20, pady=5,
            command=self.submit_form
        )
        self.create_button.place(x=670, y=850)

    def go_back(self):
        """Navigate back to the previous page."""
        if self.controller:
            self.controller.show_frame("SelectBooking")  # Replace with appropriate frame name
        else:
            print("Back button pressed (no controller linked)")

    def create_form(self):
        """Create the input form fields."""
        for i, (left_label, right_label) in enumerate(self.fields):
            left_label_widget = tk.Label(self.form_frame, text=left_label, font=("Poppins", 10), bg="#fff")
            left_label_widget.grid(row=i, column=0, padx=10, pady=5, sticky="w")

            left_entry = tk.Entry(self.form_frame, font=("Poppins", 10))
            left_entry.grid(row=i, column=1, padx=10, pady=5, sticky="ew")
            self.entry_widgets.append(left_entry)

            right_label_widget = tk.Label(self.form_frame, text=right_label, font=("Poppins", 10), bg="#fff")
            right_label_widget.grid(row=i, column=2, padx=10, pady=5, sticky="w")

            right_entry = tk.Entry(self.form_frame, font=("Poppins", 10))
            right_entry.grid(row=i, column=3, padx=10, pady=5, sticky="ew")
            self.entry_widgets.append(right_entry)

        # Adjust grid weights for responsive design
        for col in range(4):
            self.form_frame.columnconfigure(col, weight=1)

    def submit_form(self):
        """Handle the form submission."""
        form_data = [entry.get() for entry in self.entry_widgets]
        print("Form submitted with data:", form_data)
        # Add additional logic to save or process the data

    def relative_to_assets(self, path: str) -> Path:
        """Get the full path of assets relative to the ASSETS_PATH."""
        return self.ASSETS_PATH / Path(path)


# Test the CreateBooking Class
if __name__ == "__main__":
    root = tk.Tk()
    root.title("Create Booking")
    root.geometry("1440x1024")
    app = CreateBooking(root)
    app.pack(fill="both", expand=True)
    root.resizable(False, False)
    root.mainloop()
