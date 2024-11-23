import tkinter as tk
from startpage import StartPage
from homepage import HomePage
from createBooking import CreateBooking
from viewbookingpage import ViewBookingPage
from labdetailpage import LabDetailPage
from labLayoutPage import LabLayoutPage
from roomAllocation import RoomAllocation
from class_schedule_page import ClassSchedulePage
from create_class_schedule_page import CreateClassSchedulePage

from addfactspage import AddFactsPage

from roomsfactspage import RoomsFactsPage   
from addroompage import AddRoomPage
from editroompage import EditRoomPage

from lecturerfactspage import LecturersFactsPage
from addlecturerpage import AddLecturerPage
from ui.editlecturerpage import EditLecturerPage

# Static and dynamic page categories
static_pages = [StartPage, HomePage, CreateBooking, LabLayoutPage, RoomAllocation, CreateClassSchedulePage, AddFactsPage, AddRoomPage, EditRoomPage, AddLecturerPage, EditLecturerPage]

#dynamic page needs to implement init() to recreate element everytime
dynamic_pages = [LabDetailPage, ViewBookingPage, ClassSchedulePage, RoomsFactsPage, LecturersFactsPage]

class LabBookingApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Lab Booking Management System")
        self.geometry("1000x800")
        self.minsize(1000, 800)

        # Background color frame
        self.background_frame = tk.Frame(self, bg='#4CA3A3')
        self.background_frame.pack(fill='both', expand=True)

        # Responsive layout configuration
        self.background_frame.grid_rowconfigure(0, weight=1)
        self.background_frame.grid_columnconfigure(0, weight=1)

        # Active frames dictionary
        self.frames = {}

        # For optional room selection
        self.selected_room = None

        # Initialize static pages
        self.create_static_pages()

        # Start with the StartPage
        print('show farame')
        self.show_frame("StartPage")

    def create_static_pages(self):
        """Create and store static pages."""
        for Page in static_pages:
            page_name = Page.__name__
            frame = Page(self.background_frame, self)
            self.frames[page_name] = frame
            frame.grid(row=0, column=0, sticky="nsew")

    def recreate_page(self, page_name):
        """
        Recreate a dynamic page. Remove the old frame if it exists, then recreate it.
        """
        for Page in dynamic_pages:
            if Page.__name__ == page_name:
                # Remove old frame if it exists
                if page_name in self.frames:
                    self.frames[page_name].destroy()
                    del self.frames[page_name]

                # Create a new frame and store it
                frame = Page(self.background_frame, self)
                self.frames[page_name] = frame
                frame.grid(row=0, column=0, sticky="nsew")

                # Explicitly call init() to populate the page
                frame.init()
                return frame

        print(f"Error: Dynamic page '{page_name}' not found in dynamic_pages.")
        return None

    def show_frame(self, page_name):
        """
        Show the specified page. Handles both static and dynamic pages.
        """
        if page_name in [Page.__name__ for Page in dynamic_pages]:
            # Recreate and show dynamic pages
            frame = self.recreate_page(page_name)
        elif page_name in self.frames:
            # Show already-created frame
            frame = self.frames[page_name]
        else:
            print(f"Error: Page '{page_name}' not found.")
            return

        if frame:
            frame.tkraise()  # Bring frame to the top
        else:
            print(f"Error: Frame for page '{page_name}' is None.")

    def set_selected_room(self, room_name):
        """Set the selected room for navigation purposes."""
        self.selected_room = room_name


if __name__ == '__main__':
    app = LabBookingApp()
    app.mainloop()
