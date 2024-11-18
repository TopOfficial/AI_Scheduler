import tkinter as tk
from tkinter import font

class ConfirmBooking:
    def __init__(self, root):
        self.root = root
        self.root.title("Confirm Booking")
        self.root.geometry("400x600")
        self.root.configure(bg="#e0f7f7")

        self.setup_ui()

    def setup_ui(self):
        # Back Button
        back_button = tk.Button(
            self.root, text="◀ BACK", font=("Poppins", 14, "bold"), fg="white", bg="#000000",
            padx=10, pady=5, relief="flat", anchor="w", command=self.back_action
        )
        back_button.place(x=10, y=10)

        # Title
        title_label = tk.Label(
            self.root, text="Please confirm your booking details",
            font=("Poppins", 18, "bold"), fg="#a81010", bg="#e0f7f7"
        )
        title_label.pack(pady=60)

        # Booking Details
        details = [
            ("Room:", "701"),
            ("Date:", "2024-11-18"),
            ("Start Time:", "13:00"),
            ("End Time:", "15:00"),
            ("Number of People:", "20"),
            ("Name:", "Top"),
        ]

        for i, (label, value) in enumerate(details):
            tk.Label(
                self.root, text=f"{label: <15} {value}",
                font=("Poppins", 14), bg="#e0f7f7", fg="#000000"
            ).pack(anchor="w", padx=40, pady=5)

        # Confirm Button
        confirm_button = tk.Button(
            self.root, text="confirm", font=("Poppins", 14, "bold"), fg="white", bg="#007f00",
            padx=20, pady=10, relief="flat", command=self.confirm_action
        )
        confirm_button.pack(side="bottom", pady=100)

        # Cancel Button
        cancel_button = tk.Button(
            self.root, text="cancel", font=("Poppins", 14, "bold"), fg="white", bg="#000000",
            padx=20, pady=10, relief="flat", command=self.cancel_action
        )
        cancel_button.pack(side="bottom", pady=20)

    def confirm_action(self):
        print("Booking Confirmed!")

    def cancel_action(self):
        print("Booking Canceled!")

    def back_action(self):
        print("Back Button Pressed!")

# Main application
if __name__ == "__main__":
    root = tk.Tk()
    app = ConfirmBooking(root)
    root.mainloop()
