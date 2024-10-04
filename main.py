from pyswip import Prolog
from tabulate import tabulate

# prolog = Prolog()
# prolog.consult("meeting_scheduler_csp.pl")

# def find_best_time(participants):
#     query = f"find_best_time(BestTime, {participants})"
#     print(query)
#     result = list(prolog.query(query))
#     if result:
#         return result[0]['BestTime']
#     return None

# def resolve_conflicts(meeting_time, participants):
#     query = f"resolve_conflicts({meeting_time}, {participants})"
#     print(query)
#     list(prolog.query(query))

# def add_availability(person, time):
#     query = f"add_availability({person}, {time})"
#     print(query)
#     list(prolog.query(query))

# def remove_availability(person, time):
#     query = f"remove_availability({person}, {time})"
#     print(query)
#     list(prolog.query(query))

# # if __name__ == "__main__":
# #     participants = "[top, mary, susan]"
    
# #     # Finding the best meeting time
# #     best_time = find_best_time(participants)
# #     print(f"Best meeting time: {best_time}")
    
# #     resolve_conflicts(11, participants)
    
# #     add_availability('john', 13)
    
# #     remove_availability('susan', 11)
    
# #     best_time = find_best_time(participants)
# #     print(f"Best meeting time: {best_time}")


# if __name__ == "__main__":
#     participants = "[top, preme, tawan]"
    
#     add_availability('top', 14)
    
#     remove_availability('tawan', 11)



#######################################################################################

# def add_new_member(participant):
#     # Check if the member already exists
#     query_check = f"member_exists({participant})"
#     exists = list(prolog.query(query_check))

#     if exists:
#         print(f"{participant} already exists in the system.")
#         return
    
#     # Add the new member
#     query = f"add_new_member({participant})"
#     print(f"Adding new member with query: {query}")
#     result = list(prolog.query(query))  # Execute the query to add a new member
#     if result:
#         print(f"Successfully added new member: {participant}")
#     else:
#         print(f"Failed to add new member: {participant}")

#     # Save the updated availability data to the file
#     save_query = "save_availability"
#     print(f"Saving availability with query: {save_query}")
#     list(prolog.query(save_query))  # Execute the save query
#     print("Availability saved.")
    
    
# # get all members
# def get_all_members():
#     query = "get_all_members(Members)"
#     result = list(prolog.query(query))
#     if result:
#         members = result[0]['Members']
#         print(f"Current members: {', '.join(members)}")
#     else:
#         print("No members found.")



# # Function to find the best meeting time
# def find_best_meeting_time(participants, duration, day):
#     participants_str = "[" + ", ".join(participants) + "]"
#     query = f"find_best_meeting_time({participants_str}, {duration}, {day}, BestStart, BestEnd)"
#     print(f"Querying Prolog with: {query}")  # Debugging statement
#     result = list(prolog.query(query))
#     if result:
#         start_time = result[0]['BestStart']
#         end_time = result[0]['BestEnd']
#         return start_time, end_time
#     return None

# def add_availability_for_time(participant, day, time):
#     # Add single quotes around the participant name if it contains spaces
#     if ' ' in participant:
#         participant = f"'{participant}'"
    
#     query = f"add_availability({participant}, {day}, {time})"
#     print(f"Adding availability with query: {query}")
#     result = list(prolog.query(query))  # Execute the Prolog query to add availability
#     if result:
#         print(f"Successfully added {time}:00 for {participant} on {day}.")
#     else:
#         print(f"Failed to add {time}:00 for {participant} on {day}.")

#     # After adding availability, save the updated data to the file
#     save_query = "save_availability"
#     print(f"Saving availability with query: {save_query}")
#     list(prolog.query(save_query))  # Execute the save query
#     print("Availability saved.")

# def remove_availability_for_time(participants, day, start_time, end_time):
#     for participant in participants:
#         for time in range(start_time, end_time + 1):
#             query = f"remove_availability({participant}, {day}, {time})"
#             print(f"Removing availability with query: {query}")
#             result = list(prolog.query(query))  # Execute the query to remove availability
#             if result:
#                 print(f"Successfully processed {time}:00 for {participant} on {day}")
#             else:
#                 print(f"Failed to process {time}:00 for {participant} on {day}")

#     # After modifying availability, save the updated data to the file
#     save_query = "save_availability"
#     print(f"Saving availability with query: {save_query}")
#     list(prolog.query(save_query))  # Execute the save query
#     print("Availability saved.")

    
#     # After modifying availability, save the updated data to the file
#     save_query = "save_availability"
#     print(f"Saving availability with query: {save_query}")
#     list(prolog.query(save_query))  # Execute the save query
#     print("Availability saved.")
    
# def reset_all_availability():
#     query = "reset_all_availability"
#     print(f"Resetting all availability with query: {query}")
#     result = list(prolog.query(query))  # Execute the reset query
#     if result:
#         print("Successfully reset all members' availability.")
#     else:
#         print("Failed to reset availability.")
    
#     # Save the reset availability to the file
#     save_query = "save_availability"
#     print(f"Saving availability with query: {save_query}")
#     list(prolog.query(save_query))  # Execute the save query
#     print("Availability saved.")



# Main function to handle meeting booking
# if __name__ == "__main__":
    # participants = ['top', 'preme', 'tawan']
    # duration = 3  # Example duration in hours
    # day = 'monday'  # Example day
    
    # add member
    # participant = input("Enter the participant name: ").strip()
    # add_new_member(participant)
    
    # get_all_members()
    # reset_all_availability()
    
    # # For add availability
    # participant = input("Enter the participant name: ").strip()
    # day = input("Enter the day (e.g., monday, tuesday): ").strip().lower()
    # time = int(input("Enter the time (hour in 24-hour format, e.g., 9 for 9:00 AM): ").strip())
    # add_availability_for_time(participant, day, time)
    
    # best_time = find_best_meeting_time(participants, duration, day)
    
    # if best_time:
    #     start_time, end_time = best_time
    #     print(f"Best available meeting time: {start_time}:00 to {end_time}:00 on {day.capitalize()}")
        
    #     #Ask if the user wants to confirm the time
    #     lock_in = input("Do you want to confirm booking this time? (yes/no): ").strip().lower()
        
    #     if lock_in == 'yes':
    #         # Ask who is booking the time slot
    #         booked_by = input("Who is booking this meeting? ").strip()
            
    #         # Remvoe availability
    #         remove_availability_for_time(participants, day, start_time, end_time)
            
    #         # Display result
    #         print(f"{booked_by} has booked a meeting with {', '.join(participants)} "
    #               f"from {start_time}:00 to {end_time}:00 on {day.capitalize()}.")
    #     else:
    #         print("The meeting was not booked.")
    # else:
    #     print("No available time slot could be found.")
    
    
#######################################################################################

AVAILABLE_PATH = "prolog/meeting_scheduler_csp.pl"
RECORDS_PATH = "prolog/meeting_data.pl"

class MeetingScheduler:
    def __init__(self):
        # Initialize Prolog and consult the required file
        self.prolog = Prolog()
        self.prolog.consult(AVAILABLE_PATH)
        self.prolog.consult(RECORDS_PATH)

    # Add a new member to the system
    def add_new_member(self, participant):
        participant = self._format_participant(participant)
        
        # Check if the member already exists
        query_check = f"member_exists({participant})"
        exists = list(self.prolog.query(query_check))

        if exists:
            print(f"{participant} already exists in the system.")
            return

        # Add the new member
        query = f"add_new_member({participant})"
        print(f"Adding new member with query: {query}")
        result = list(self.prolog.query(query))
        if result:
            print(f"Successfully added new member: {participant}")
        else:
            print(f"Failed to add new member: {participant}")

        # Save the updated availability data
        self._save_availability()

    # Get all current members
    def get_all_members(self):
        query = "get_all_members(Members)"
        result = list(self.prolog.query(query))
        if result:
            members = result[0]['Members']
            print(f"Current members: {', '.join(members)}")
        else:
            print("No members found.")

    # Find the best meeting time
    def find_best_meeting_time(self, participants, duration, day):
        participants_str = "[" + ", ".join(self._format_participants(participants)) + "]"
        query = f"find_best_meeting_time({participants_str}, {duration}, {day}, BestStart, BestEnd)"
        print(f"Querying Prolog with: {query}")
        result = list(self.prolog.query(query))
        if result:
            start_time = result[0]['BestStart']
            end_time = result[0]['BestEnd']
            return start_time, end_time
        return None

        
    # Add availability for a range of times for multiple participants
    def add_availability_for_time(self, participants, day, start_time, end_time):
        for participant in self._format_participants(participants):
            for time in range(start_time, end_time + 1):
                query = f"add_availability({participant}, {day}, {time})"
                print(f"Adding availability with query: {query}")
                result = list(self.prolog.query(query))
                if result:
                    print(f"Successfully added {time}:00 for {participant} on {day}.")
                else:
                    print(f"Failed to add {time}:00 for {participant} on {day}.")

        # Save the updated availability
        self._save_availability()

    # Remove availability for a range of times for multiple participants
    def remove_availability_for_time(self, participants, day, start_time, end_time):
        for participant in self._format_participants(participants):
            for time in range(start_time, end_time + 1):
                query = f"remove_availability({participant}, {day}, {time})"
                print(f"Removing availability with query: {query}")
                result = list(self.prolog.query(query))
                if result:
                    print(f"Successfully processed {time}:00 for {participant} on {day}")
                else:
                    print(f"Failed to process {time}:00 for {participant} on {day}")

        # Save the updated availability
        self._save_availability()

    # Reset all members' availability
    def reset_all_availability(self):
        query = "reset_all_availability"
        print(f"Resetting all availability with query: {query}")
        result = list(self.prolog.query(query))
        if result:
            print("Successfully reset all members' availability.")
        else:
            print("Failed to reset availability.")
        
        # Save the reset availability
        self._save_availability()
        
        self._reset_meeting_records()
        
    def _reset_meeting_records(self):
        # Open the meeting records file and clear its contents
        with open(RECORDS_PATH, 'w') as file:
            file.write("")  # Writing an empty string to the file effectively clears it
        print(f"Meeting records in {RECORDS_PATH} have been cleared.")

    # Save the updated availability to a file
    def _save_availability(self):
        save_query = "save_availability"
        print(f"Saving availability with query: {save_query}")
        list(self.prolog.query(save_query))
        print("Availability saved.")

    # Helper function to format participant names
    def _format_participant(self, participant):
        if ' ' in participant:
            participant = f"'{participant}'"
        return participant

    # Helper function to format multiple participants
    def _format_participants(self, participants):
        return [self._format_participant(participant) for participant in participants]
    
    def record_meeting(self, booked_by, participants, day, start_time, end_time):
        # Prepare the participants list for Prolog
        participants_str = "[" + ", ".join(participants) + "]"

        # Format the fact to be written to the Prolog file
        meeting_fact = f"meeting({booked_by}, {participants_str}, {day}, {start_time}, {end_time}).\n"

        # Append the fact to the 'meeting_data.pl' file
        with open(RECORDS_PATH, 'a') as file:
            file.write(meeting_fact)

        print(f"Meeting recorded: {booked_by} with {', '.join(participants)} on {day.capitalize()} from {start_time}:00 to {end_time}:00.")

    def get_meetings_for_member(self, member):
        query = f"find_meetings_for_member({member}, BookedBy, Participants, Day, StartTime, EndTime)"
        result = list(self.prolog.query(query))
        
        if result:
            print(f"Meetings for {member}:")
            for meeting in result:
                booked_by = meeting['BookedBy']
                participants = meeting['Participants']
                day = meeting['Day']
                start_time = meeting['StartTime']
                end_time = meeting['EndTime']
                print(f"Booked by {booked_by}, Participants: {participants}, Day: {day}, Time: {start_time}:00 - {end_time}:00")
        else:
            print(f"No meetings found for {member}.")
            
    def member_exists(self, member):
        member = self._format_participant(member)
        query = f"member_exists({member})"
        result = list(self.prolog.query(query))
        return len(result) > 0  # Return True if the member exists, otherwise False

    # Method to check if multiple members exist
    def check_members_exist(self, members):
        non_existing_members = []
        for member in members:
            if not self.member_exists(member):
                non_existing_members.append(member)
        
        if non_existing_members:
            print(f"The following members do not exist: {', '.join(non_existing_members)}")
            return False
        print("All members exist.")
        return True

    # Helper function to format participant names for Prolog
    def _format_participant(self, participant):
        if ' ' in participant:
            return f"'{participant}'"
        return participant
    
    
    def print_all_meetings(self):
        query = "meeting(BookedBy, Participants, Day, StartTime, EndTime)"
        result = list(self.prolog.query(query))
        
        if not result:
            print("No meetings found.")
            return
        
        # Do later

# Example usage
if __name__ == "__main__":
    scheduler = MeetingScheduler()
    
    weekdays = ['monday', 'tuesday', 'wednesday', 'thursday', 'friday']
    weekends = ['saturday', 'sunday']
    
    action = input("Do you want to \n(1) book a meeting, \n(2) add availability, \n(3) remove availability, \n(4) add a new member, \n(5) get member meeting timeslot, \n(6) get all current members, \n(7) reset all availability? \n(8) print all meetings \nCHOICE: ").strip()

    if action == "1":
        # Example participants for booking
        # participants = ['top', 'preme', 'tawan']
        # duration = 3  # Example duration in hours
        # day = 'monday'  # Example day
        
        participants = input("Enter the name of participants (separated by commas): ").split(", ")
        
        if scheduler.check_members_exist(participants):
            print("All participants exist. Proceeding with scheduling.")
            # Proceed with further actions (e.g., scheduling a meeting)
        else:
            print("Some participants do not exist. Please add them before proceeding.")
            exit()
            
        duration = int(input("Enter the duration of the meeting (in hours): ").strip())
        
        while True:
            day = input("Enter the day of the meeting (e.g., monday, tuesday): ").strip().lower()
            
            if day in weekdays:
                print(f"The meeting is scheduled on {day.capitalize()}.")
                break
            elif day in weekends:
                print(f"{day.capitalize()} is a company holiday.")
                break
            else:
                print("Invalid input. Please enter a valid weekday (e.g., Monday, Tuesday).")
        
        best_time = scheduler.find_best_meeting_time(participants, duration, day)

        if best_time:
            start_time, end_time = best_time
            print(f"Best available meeting time: {start_time}:00 to {end_time}:00 on {day.capitalize()}")
            lock_in = input("Do you want to lock in this time? (yes/no): ").strip().lower()

            if lock_in == 'yes':
                booked_by = input("Who is booking this meeting? ").strip()
                scheduler.remove_availability_for_time(participants, day, start_time, end_time)
                print(f"{booked_by} has booked a meeting with {', '.join(participants)} "
                      f"from {start_time}:00 to {end_time}:00 on {day.capitalize()}.")
                
                scheduler.record_meeting(booked_by, participants, day, start_time, end_time)
                
            else:
                print("The meeting was not booked.")
        else:
            print("No available time slot could be found.")

    elif action == "2":
        participants = input("Enter the participant names (comma-separated): ").strip().split(",")
        day = input("Enter the day (e.g., monday, tuesday): ").strip().lower()
        start_time = int(input("Enter the start time (hour in 24-hour format): ").strip())
        end_time = int(input("Enter the end time (hour in 24-hour format): ").strip())
        scheduler.add_availability_for_time(participants, day, start_time, end_time)
        
    # remove availability
    elif action == "3":
        participants = input("Enter the participant names (comma-separated): ").strip().split(",")
        day = input("Enter the day (e.g., monday, tuesday): ").strip().lower()
        start_time = int(input("Enter the start time (hour in 24-hour format): ").strip())
        end_time = int(input("Enter the end time (hour in 24-hour format): ").strip())
        scheduler.remove_availability_for_time(participants, day, start_time, end_time)

    elif action == "4":
        new_member = input("Enter the name of the new member: ").strip()
        scheduler.add_new_member(new_member)
        
    elif action == "5":
        member = input("Enter the member name: ").strip()
        print(member)
        scheduler.get_meetings_for_member(member)

    elif action == "6":
        scheduler.get_all_members()

    elif action == "7":
        scheduler.reset_all_availability()
        
    elif action == "8":
        scheduler.print_all_meetings()


    # participants = ['top', 'preme', 'tawan']
    # day = 'friday'
    # start_time = 13
    # end_time = 17
    # booked_by = 'top'
    # scheduler.record_meeting(booked_by, participants, day, start_time, end_time)