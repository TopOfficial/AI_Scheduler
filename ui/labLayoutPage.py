# import tkinter as tk
# from reactButton import RectButton


# class LabLayoutPage(tk.Frame):
#     def __init__(self, parent, controller):
#         super().__init__(parent, bg="#e0f7f9")
#         self.controller = controller
#         self.create_layout()

#     def create_layout(self):
#         # Create canvas for layout
#         canvas = tk.Canvas(self, bg="#e0f7f9", highlightthickness=0)
#         canvas.pack(fill=tk.BOTH, expand=True)

#         # Define relative positions for labs and labels
#         labs = {
#             "701": (-300, -100, -200, 0),
#             "702": (-300, 20, -200, 120),
#             "702": (-300, 140, -170, 240),
#             "704": (-150, -100, 150, 0),
#             "705": (170, -100, 270, 0),
#             "Lab06": (170, 20, 270, 120),
#             "Lab07": (-150, 140, -50, 240),
#             "Lab08": (-30, 140, 70, 240),
#             "Lab09": (90, 140, 270, 240),  # Widened Lab09
#             "Co-working space": (290, -100, 390, 120),
#             "stairs": (-200, -200, -100, -160),  # Moved stairs even farther left
#             "elevator 1": (70, -200, 150, -160),
#             "elevator 2": (170, -200, 250, -160),
#         }

#         def draw_layout(event):
#             # Get canvas dimensions
#             canvas_width = canvas.winfo_width()
#             canvas_height = canvas.winfo_height()

#             # Calculate the center of the canvas, offset slightly to the left
#             center_x = canvas_width // 2 - 50  # Adjusted to move 50 pixels left
#             center_y = canvas_height // 2

#             # Clear canvas before redrawing
#             canvas.delete("all")

#             # Add labs to canvas
#             for label, rel_coords in labs.items():
#                 color = "#ffdca5" if label == "Co-working space" else "white"
#                 abs_coords = (
#                     center_x + rel_coords[0],
#                     center_y + rel_coords[1],
#                     center_x + rel_coords[2],
#                     center_y + rel_coords[3],
#                 )
#                 canvas.create_rectangle(*abs_coords, fill=color, outline="black")
#                 x, y = (abs_coords[0] + abs_coords[2]) // 2, (abs_coords[1] + abs_coords[3]) // 2
#                 canvas.create_text(x, y, text=label,fill="#17252A", font=("Arial", 10, "bold"))

#             # Add 'Lab Layout' title (bigger and moved down)
#             canvas.create_text(
#                 canvas_width // 2, 98,  # Move the title closer to the layout (vertical position)
#                 text="Lab Layout",
#                 font=("Arial", 36, "bold"),  # Increased font size
#                 fill="#17252A"
#             )

#         # Bind resize event to redraw layout
#         canvas.bind("<Configure>", draw_layout)

#         # Back button
#         self.back_button = RectButton(
#             self,
#             text="← BACK",
#             command=self.go_back,
#             width=120,
#             height=40,
#             bg_color="#17252A",
#             fg_color="#FEFFFF",
#             font=("Poppins", 12, "bold"),
#         )
#         self.back_button.place(x=20, y=20)

#     def go_back(self):
#         """Navigate back to the previous page."""
#         self.controller.show_frame("HomePage")  # Replace with the desired page

import tkinter as tk
from tkinter import PhotoImage
from PIL import Image, ImageTk  # For image resizing
from reactButton import RectButton


class LabLayoutPage(tk.Frame):
    def __init__(self, parent, controller):
        super().__init__(parent, bg="#e0f7f9")
        self.controller = controller

        # Store the floor-specific image paths
        self.floor_images = {
            7: "ui/floor_7.png",
            8: "ui/floor_8.png",
        }
        self.current_floor = 7  # Default floor is 7
        self.loaded_images = {}  # Cache resized images
        self.create_layout()

    def create_layout(self):
        # Create canvas for layout
        self.canvas = tk.Canvas(self, bg="#e0f7f9", highlightthickness=0)
        self.canvas.pack(fill=tk.BOTH, expand=True)

        # Floor switching buttons
        self.floor_7_button = RectButton(
            self,
            text="Floor 7",
            command=lambda: self.switch_floor(7),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.floor_7_button.place(x=700, y=20)

        self.floor_8_button = RectButton(
            self,
            text="Floor 8",
            command=lambda: self.switch_floor(8),
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.floor_8_button.place(x=850, y=20)

        # Back button
        self.back_button = RectButton(
            self,
            text="← BACK",
            command=self.go_back,
            width=120,
            height=40,
            bg_color="#17252A",
            fg_color="#FEFFFF",
            font=("Poppins", 12, "bold"),
        )
        self.back_button.place(x=20, y=20)

        # Bind resize event to redraw layout
        self.canvas.bind("<Configure>", self.draw_layout)

    def draw_layout(self, event=None):
        """Draw the layout of the current floor."""
        # Get canvas dimensions
        canvas_width = self.canvas.winfo_width() - 50  # Adjusted to leave space for buttons
        canvas_height = self.canvas.winfo_height() + 70   # Adjusted to leave space for buttons

        # Clear canvas before redrawing
        self.canvas.delete("all")

        # Add floor-specific image in the center, resized to fit
        if self.current_floor in self.floor_images:
            image_path = self.floor_images[self.current_floor]
            resized_image = self.get_resized_image(image_path, canvas_width, canvas_height)
            if resized_image:
                self.canvas.create_image(
                    canvas_width // 2,
                    canvas_height // 2,
                    anchor=tk.CENTER,
                    image=resized_image,
                )
                # Keep a reference to prevent garbage collection
                self.loaded_images[self.current_floor] = resized_image

        # Add 'Lab Layout' title
        self.canvas.create_text(
            canvas_width // 2, 120,  # Centered near the top
            text=f"Lab Layout - Floor {self.current_floor}",
            font=("Arial", 36, "bold"),
            fill="#17252A",
        )

    def get_resized_image(self, image_path, max_width, max_height):
        """Resize an image to fit within the specified dimensions while maintaining aspect ratio."""
        try:
            # Open the image with PIL
            image = Image.open(image_path)
            # Calculate the aspect ratio
            aspect_ratio = image.width / image.height
            if image.width > max_width or image.height > max_height:
                # Resize while maintaining aspect ratio
                if max_width / max_height < aspect_ratio:
                    new_width = max_width
                    new_height = int(max_width / aspect_ratio)
                else:
                    new_height = max_height
                    new_width = int(max_height * aspect_ratio)
            else:
                # Use original size if smaller than canvas
                new_width, new_height = image.width, image.height

            # Resize the image using LANCZOS for high quality
            resized_image = image.resize((new_width, new_height), Image.Resampling.LANCZOS)
            return ImageTk.PhotoImage(resized_image)
        except Exception as e:
            print(f"Error loading image {image_path}: {e}")
            return None


    def switch_floor(self, floor):
        """Switch to a different floor layout."""
        if floor in self.floor_images:
            self.current_floor = floor
            self.draw_layout()

    def go_back(self):
        """Navigate back to the previous page."""
        self.controller.show_frame("HomePage")
