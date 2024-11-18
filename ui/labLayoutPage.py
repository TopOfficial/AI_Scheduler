import tkinter as tk
from reactButton import RectButton


class LabLayoutPage(tk.Frame):
    def __init__(self, parent, controller):
        super().__init__(parent, bg="#e0f7f9")
        self.controller = controller
        self.create_layout()

    def create_layout(self):
        # Create canvas for layout
        canvas = tk.Canvas(self, bg="#e0f7f9", highlightthickness=0)
        canvas.pack(fill=tk.BOTH, expand=True)

        # Define relative positions for labs and labels
        labs = {
            "Lab01": (-300, -100, -200, 0),
            "Lab02": (-300, 20, -200, 120),
            "Lab03": (-300, 140, -170, 240),
            "Lab04": (-150, -100, 150, 0),
            "Lab05": (170, -100, 270, 0),
            "Lab06": (170, 20, 270, 120),
            "Lab07": (-150, 140, -50, 240),
            "Lab08": (-30, 140, 70, 240),
            "Lab09": (90, 140, 270, 240),  # Widened Lab09
            "Co-working space": (290, -100, 390, 120),
            "stairs": (-200, -200, -100, -160),  # Moved stairs even farther left
            "elevator 1": (70, -200, 150, -160),
            "elevator 2": (170, -200, 250, -160),
        }

        def draw_layout(event):
            # Get canvas dimensions
            canvas_width = canvas.winfo_width()
            canvas_height = canvas.winfo_height()

            # Calculate the center of the canvas, offset slightly to the left
            center_x = canvas_width // 2 - 50  # Adjusted to move 50 pixels left
            center_y = canvas_height // 2

            # Clear canvas before redrawing
            canvas.delete("all")

            # Add labs to canvas
            for label, rel_coords in labs.items():
                color = "#ffdca5" if label == "Co-working space" else "white"
                abs_coords = (
                    center_x + rel_coords[0],
                    center_y + rel_coords[1],
                    center_x + rel_coords[2],
                    center_y + rel_coords[3],
                )
                canvas.create_rectangle(*abs_coords, fill=color, outline="black")
                x, y = (abs_coords[0] + abs_coords[2]) // 2, (abs_coords[1] + abs_coords[3]) // 2
                canvas.create_text(x, y, text=label,fill="#17252A", font=("Arial", 10, "bold"))

            # Add 'Lab Layout' title (bigger and moved down)
            canvas.create_text(
                canvas_width // 2, 98,  # Move the title closer to the layout (vertical position)
                text="Lab Layout",
                font=("Arial", 36, "bold"),  # Increased font size
                fill="#17252A"
            )

        # Bind resize event to redraw layout
        canvas.bind("<Configure>", draw_layout)

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

    def go_back(self):
        """Navigate back to the previous page."""
        self.controller.show_frame("HomePage")  # Replace with the desired page
