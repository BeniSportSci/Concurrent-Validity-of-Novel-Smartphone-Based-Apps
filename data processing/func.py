def calculate_velocity(marker_data):
    start_frame = None
    in_concentric = False

    for idx, position in enumerate(marker_data):
        if idx == 0:
            prev_pos = position
            continue

        # Calculate the difference in position
        position_diff = position[2] - prev_pos[2]

        # Check for significant movement
        if position_diff > MOVEMENT_THRESHOLD_MM:
            # If we are not already in a concentric phase, start one, else continue
            if not in_concentric:
                in_concentric = True
                start_frame = idx
            else:
                continue
        else:
            # If we are in a concentric phase, end it
            if in_concentric:
                in_concentric = False
                range_of_motion = position_diff

                # Check if movement exceeds minimum threshold
                if range_of_motion > MIN_MOVEMENT_MM:
                    # If so, calculate the duration and mean velocity
                    duration = idx - start_frame
                    duration_s = duration / FPS
                    mean_velocity_mm = range_of_motion / duration * FPS
                    mean_velocity = mean_velocity_mm / 1000
                    return mean_velocity, range_of_motion, duration_s

        prev_pos = position

    # Return None if no significant movement is detected
    return None, None, None
