import tkinter as tk
from startpage import StartPage
from homepage import HomePage
from booking import SelectBooking
from createBooking import CreateBooking  # Import the new CreateBooking class

# Add new pages here
pages = [StartPage, HomePage, SelectBooking, CreateBooking]

class LabBookingApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Lab Booking Management System")
        self.root.geometry("1000x800")
        self.root.minsize(1000, 800)

        # Use a frame to set the background color
        self.background_frame = tk.Frame(self.root, bg='#4CA3A3')
        self.background_frame.pack(fill='both', expand=True)

        # Configure grid layout for the background_frame to expand
        self.background_frame.grid_rowconfigure(0, weight=1)
        self.background_frame.grid_columnconfigure(0, weight=1)

        # Store frames in a dictionary
        self.frames = {}
        self.selected_room = None  # Store the selected room name

        # Create pages (frames) for the app
        for F in pages:
            page_name = F.__name__
            frame = F(self.background_frame, self)  # Pass the controller
            self.frames[page_name] = frame
            frame.grid(row=0, column=0, sticky="nsew")  # Position the frame in grid

        # Show the initial page (StartPage)
        self.show_frame("StartPage")

    def show_frame(self, page_name):
        """Switch to the specified page."""
        frame = self.frames.get(page_name)
        if frame:
            frame.tkraise()  # Raise the selected frame to the top
        else:
            print(f"Error: Page '{page_name}' not found in frames.")

if __name__ == '__main__':
    root = tk.Tk()
    app = LabBookingApp(root)
    root.mainloop()
