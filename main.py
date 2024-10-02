# from pyswip import Prolog

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


from pyswip import Prolog

# Initialize Prolog
prolog = Prolog()
prolog.consult("prolog/meeting_scheduler_csp.pl")

def add_new_member(participant):
    # Check if the member already exists
    query_check = f"member_exists({participant})"
    exists = list(prolog.query(query_check))

    if exists:
        print(f"{participant} already exists in the system.")
        return
    
    # Add the new member
    query = f"add_new_member({participant})"
    print(f"Adding new member with query: {query}")
    result = list(prolog.query(query))  # Execute the query to add a new member
    if result:
        print(f"Successfully added new member: {participant}")
    else:
        print(f"Failed to add new member: {participant}")

    # Save the updated availability data to the file
    save_query = "save_availability"
    print(f"Saving availability with query: {save_query}")
    list(prolog.query(save_query))  # Execute the save query
    print("Availability saved.")
    
    
# get all members
def get_all_members():
    query = "get_all_members(Members)"
    result = list(prolog.query(query))
    if result:
        members = result[0]['Members']
        print(f"Current members: {', '.join(members)}")
    else:
        print("No members found.")



# Function to find the best meeting time
def find_best_meeting_time(participants, duration, day):
    participants_str = "[" + ", ".join(participants) + "]"
    query = f"find_best_meeting_time({participants_str}, {duration}, {day}, BestStart, BestEnd)"
    print(f"Querying Prolog with: {query}")  # Debugging statement
    result = list(prolog.query(query))
    if result:
        start_time = result[0]['BestStart']
        end_time = result[0]['BestEnd']
        return start_time, end_time
    return None

def add_availability_for_time(participant, day, time):
    query = f"add_availability({participant}, {day}, {time})"
    print(f"Adding availability with query: {query}")
    result = list(prolog.query(query))  # Execute the Prolog query to add availability
    if result:
        print(f"Successfully added {time}:00 for {participant} on {day}.")
    else:
        print(f"Failed to add {time}:00 for {participant} on {day}.")

    # After adding availability, save the updated data to the file
    save_query = "save_availability"
    print(f"Saving availability with query: {save_query}")
    list(prolog.query(save_query))  # Execute the save query
    print("Availability saved.")

def remove_availability_for_time(participants, day, start_time, end_time):
    for participant in participants:
        for time in range(start_time, end_time + 1):
            query = f"remove_availability({participant}, {day}, {time})"
            print(f"Removing availability with query: {query}")
            result = list(prolog.query(query))  # Execute the query to remove availability
            if result:
                print(f"Successfully processed {time}:00 for {participant} on {day}")
            else:
                print(f"Failed to process {time}:00 for {participant} on {day}")

    # After modifying availability, save the updated data to the file
    save_query = "save_availability"
    print(f"Saving availability with query: {save_query}")
    list(prolog.query(save_query))  # Execute the save query
    print("Availability saved.")

    
    # After modifying availability, save the updated data to the file
    save_query = "save_availability"
    print(f"Saving availability with query: {save_query}")
    list(prolog.query(save_query))  # Execute the save query
    print("Availability saved.")
    
def reset_all_availability():
    query = "reset_all_availability"
    print(f"Resetting all availability with query: {query}")
    result = list(prolog.query(query))  # Execute the reset query
    if result:
        print("Successfully reset all members' availability.")
    else:
        print("Failed to reset availability.")
    
    # Save the reset availability to the file
    save_query = "save_availability"
    print(f"Saving availability with query: {save_query}")
    list(prolog.query(save_query))  # Execute the save query
    print("Availability saved.")



# Main function to handle meeting booking
if __name__ == "__main__":
    participants = ['top', 'preme', 'tawan']
    duration = 3  # Example duration in hours
    day = 'monday'  # Example day
    
    # add member
    participant = input("Enter the participant name: ").strip()
    add_new_member(participant)
    
    # get_all_members()
    # reset_all_availability()
    
    # For add availability
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