import tkinter as tk

class RoundButton(tk.Canvas):
    def __init__(self, parent, text, command=None, radius=20, bg="#000000", fg="#FFFFFF", **kwargs):
        super().__init__(parent, height=radius*2, width=radius*5, bg=parent['bg'], highlightthickness=0, **kwargs)

        # Draw rounded rectangle
        self.radius = radius
        self.command = command
        self.bg = bg
        self.fg = fg

        self.create_oval(0, 0, 2*radius, 2*radius, fill=bg, outline=bg)  # Left circle
        self.create_oval((5*radius)-2*radius, 0, 5*radius, 2*radius, fill=bg, outline=bg)  # Right circle
        self.create_rectangle(radius, 0, (5*radius)-radius, 2*radius, fill=bg, outline=bg)  # Middle rectangle

        # Add text to the center
        self.text = self.create_text((2.5*radius), radius, text=text, font=("Helvetica", 14, "bold"), fill=fg)

        # Bind click event
        if command:
            self.bind("<Button-1>", lambda event: command())

    def configure(self, **kwargs):
        if "text" in kwargs:
            self.itemconfig(self.text, text=kwargs.pop("text"))
        super().configure(**kwargs)
