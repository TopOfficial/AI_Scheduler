import tkinter as tk
from startpage import StartPage
from homepage import HomePage
from booking import SelectBooking
from createBooking import CreateBooking

# Add new pages here
pages = [StartPage, HomePage, SelectBooking, CreateBooking]

class LabBookingApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Lab Booking Management System")
        self.geometry("1000x800")
        self.minsize(1000, 800)

        # Use a frame to set the background color
        self.background_frame = tk.Frame(self, bg='#4CA3A3')
        self.background_frame.pack(fill='both', expand=True)

        # Configure grid layout for the background_frame to expand
        self.background_frame.grid_rowconfigure(0, weight=1)
        self.background_frame.grid_columnconfigure(0, weight=1)

        # Store frames in a dictionary
        self.frames = {}
        self.selected_room = None  # Store the selected room name for navigation

        # Create and add pages (frames) to the app
        self.create_pages()

        # Show the initial page (StartPage)
        self.show_frame("StartPage")

    def create_pages(self):
        """Create all pages and store them in a dictionary."""
        for Page in pages:
            page_name = Page.__name__
            frame = Page(self.background_frame, self)  # Pass the controller
            self.frames[page_name] = frame
            frame.grid(row=0, column=0, sticky="nsew")  # Position the frame in grid

    def show_frame(self, page_name):
        """Switch to the specified page."""
        frame = self.frames.get(page_name)
        if frame:
            frame.tkraise()  # Raise the selected frame to the top
        else:
            print(f"Error: Page '{page_name}' not found in frames.")

    def set_selected_room(self, room_name):
        """Set the selected room name for navigation."""
        self.selected_room = room_name


if __name__ == '__main__':
    app = LabBookingApp()
    app.mainloop()
