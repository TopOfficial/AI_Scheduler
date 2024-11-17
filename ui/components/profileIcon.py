import tkinter as tk

class ProfileIcon(tk.Canvas):
    def __init__(self, parent, icon_color="black", circle_color="black", **kwargs):
        # Initialize the canvas with no visible background
        super().__init__(parent, bg=parent.cget("bg"), highlightthickness=0, **kwargs)
        self.icon_color = icon_color
        self.circle_color = circle_color
        self.draw_circular_icon()

    def draw_circular_icon(self):
        """Draws a smaller circular icon with the design inside."""
        # Draw the outer circle (frame)
        self.create_oval(2, 2, 62, 62, outline=self.circle_color, width=2)
        
        # Draw the inner icon components
        # Circle for the head
        self.create_oval(20, 10, 40, 30, fill=self.icon_color, outline=self.icon_color)

        # Semi-circle for the body
        self.create_arc(15, 30, 47, 60, start=0, extent=180, fill=self.icon_color, outline=self.icon_color)
