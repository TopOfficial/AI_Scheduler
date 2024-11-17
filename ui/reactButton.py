import tkinter as tk
import math

class RectButton(tk.Canvas):
    def __init__(self, parent, text, command=None, width=200, height=50, font=("Helvetica", 12, "bold"), bg_color="#000", fg_color="#FFF"):
        super().__init__(parent, width=width, height=height, highlightthickness=0)
        
        # Store custom options
        self.command = command
        self.bg_color = bg_color
        self.fg_color = fg_color

        # Draw the button rectangle
        self.rect = self.create_rectangle(0, 0, width, height, fill=bg_color, outline="")

        # Draw the button text
        self.text = self.create_text(width // 2, height // 2, text=text, font=font, fill=fg_color)

        # Bind events
        self.bind("<Button-1>", self.on_click)
        self.bind("<Enter>", self.on_hover)
        self.bind("<Leave>", self.on_leave)

    def on_click(self, event):
        """Invoke the button's command when clicked."""
        if self.command:
            self.command()

    def on_hover(self, event):
        """Change the button color on hover."""
        self.itemconfig(self.rect, fill="#444")  # Darker shade for hover

    def on_leave(self, event):
        """Revert the button color when the cursor leaves."""
        self.itemconfig(self.rect, fill=self.bg_color)
        
