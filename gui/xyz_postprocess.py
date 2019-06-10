from xyz import XYZ
from xyz import XYZ_Single
import subprocess
import os


class Particle:

    nextid = 1

    def __init__(self):
        self.atom = None
        self.pos = [0, 0, 0]
        self.momentum = [0, 0, 0]
        self.velocity = [0, 0, 0]
        self.mass = 0
        self.id = -1

    def fromXYZ(self, atom, value):
        self.atom = atom
        self.pos = [value[0], value[1], value[2]]
        self.momentum = [value[3], value[4], value[5]]
        self.mass = value[6]
        self.velocity = [value[3]/value[6],
                         value[4]/value[6],
                         value[5]/value[6]]


def isSame(p_a, p_b, dt, color_nearest_neighbors=False):
    if p_a == p_b:
        return True
    if p_a.atom != p_b.atom and not color_nearest_neighbors:
        return False
    # always the same for the projectile Atom
    if p_a.id == 0 and p_b.id == 0:
        return True
    
    p_a_dt = [p_a.pos[0] + p_a.velocity[0]*dt,
              p_a.pos[1] + p_a.velocity[1]*dt,
              p_a.pos[2] + p_a.velocity[2]*dt]
    
    p_b_dt = [p_b.pos[0] + p_b.velocity[0]*dt,
              p_b.pos[1] + p_b.velocity[1]*dt,
              p_b.pos[2] + p_b.velocity[2]*dt]
    
    diff = (p_a_dt[0] - p_b_dt[0])**2 + \
           (p_a_dt[1] - p_b_dt[1])**2 + \
           (p_a_dt[2] - p_b_dt[2])**2
    return diff <= .25


def merge(pset1, pset2, dt, first, color_nearest_neighbors=False):
    len1 = len(pset1)
    len2 = len(pset2)
    merged = False
    num = 0
    for i in range(len1):
        p_a = pset1[i]
        has = False
        for j in range(len2):
            p_b = pset2[j]
            if isSame(p_a, p_b, dt, color_nearest_neighbors=color_nearest_neighbors):
                has = True
                if not first:
                    break
                if p_b.id != -1:
                    p_a.id = p_b.id
                elif p_a.id != -1:
                    p_b.id = p_a.id
                else:
                    p_a.id = p_b.id = Particle.nextid
                    Particle.nextid = Particle.nextid + 1

        if not has:
            if color_nearest_neighbors:
                p_a.atom = "X"
            pset2.append(p_a)
            merged = True
            num = num + 1
    return merged, num


def xyzFromParticles(time, pset):
    xyz_single = XYZ_Single()
    xyz_single.number = len(pset)
    xyz_single.comment = str(time)+'\n'
    for particle in pset:
        xyz_single.atoms.append(particle.atom)
        value = []
        value.append(particle.pos[0])
        value.append(particle.pos[1])
        value.append(particle.pos[2])
        value.append(particle.momentum[0])
        value.append(particle.momentum[1])
        value.append(particle.momentum[2])
        value.append(particle.mass)
        xyz_single.values.append(value)
    return xyz_single


def linear_approximate(approximation_time, previous_time, previous_values, next_time, next_values):
    """
    :param approximation_time: The time that our approximation will occur at
    :param previous_time: The time of the state before our approximation that cooresponds to previous_values
    :param previous_values: The values that occur before our approximate state
    :param next_time: The time of the state after our approximation that coorespond to next_values
    :param next_values: The values that occur after our approximation state that we are approximating
    :return: approximate values: The linear approximation of the state between previous_time and next_time
    """
    approximate_values = []
    for index in range(len(previous_values)):
        slope = (next_values[index] - previous_values[index])/(next_time - previous_time)
        value_change = slope*(approximation_time - previous_time)
        approximate_value = previous_values[index] + value_change
        approximate_values.append(approximate_value)
    return approximate_values


def get_next_state(time, time_array, state_array):
    """
    :param time: time that we wish to create the framerate for
    :param time_array: the times array of all the times of the different states
    :param state_array: the states that this system takes over the course of the interaction, it is a list of lists of
    particles
    :return: new_state: the state for the given time
    """
    # Find the index of the state that occurs just after the time we are creating a state for
    index_after_new_state = len(time_array) - 1
    for index, state_time in enumerate(time_array):
        if state_time > time:
            index_after_new_state = index
            break
    # If the first time is before the first state in our array, we just return the first state
    if index_after_new_state == 0:
        return state_array[0]
    new_state = []
    # Iterate through the particles in the state and create the new frame from previous and next particle
    for index in range(len(state_array[0])):
        current_particle = state_array[index_after_new_state - 1][index]
        next_particle = state_array[index_after_new_state][index]
        approximate_particle = Particle()
        approximate_particle.atom = current_particle.atom
        approximate_particle.id = current_particle.id
        approximate_particle.mass = current_particle.mass
        approximate_particle.pos = linear_approximate(time, time_array[index_after_new_state-1],
                                                      current_particle.pos,
                                                      time_array[index_after_new_state], next_particle.pos)
        approximate_particle.velocity = linear_approximate(time, time_array[index_after_new_state - 1],
                                                           current_particle.velocity,
                                                           time_array[index_after_new_state], next_particle.velocity)
        approximate_particle.momentum = linear_approximate(time, time_array[index_after_new_state - 1],
                                                           current_particle.momentum,
                                                           time_array[index_after_new_state], next_particle.momentum)
        new_state.append(approximate_particle)
    return new_state


def smooth_framerate(times, particles):
    """
    Algorithm:
    Find Minimum frametime
    Space out the video using that frametime from the beginning
    Calculate the location of the particle as the linear approximation using the particle locations in the two frames
    on either side of it

    parameters:
    times : [float]
        list of times
    particles : [[particle]]
        list of frames where each frame is a list of particles. A particle contains that particles location and velocity

    returns:
    times: [float]
        the times array but with a smooth framerate equal to the minimum framerate above
    particles: [[particle]]
        the particle array but with a coorespondingly smooth framerate
    """
    print("Begining smoothing")
    # get minimum timestep
    minimum_timestep = times[-1]
    minimum_index = 0
    for index in range(20, len(times) - 1):
        timestep = times[index + 1] - times[index]
        if timestep < minimum_timestep:
            minimum_timestep = timestep
            minimum_index = index
    for index, item in enumerate(particles):
        if len(item) != len(particles[0]):
            print(len(item))
            print(index)
    minimum_timestep = max(minimum_timestep, times[-1]/1000)
    new_times_array = []
    new_particles_array = []
    for step in range(int(times[-1]/minimum_timestep)):
        next_time = step*minimum_timestep
        next_state = get_next_state(next_time, times, particles)
        new_times_array.append(next_time)
        new_particles_array.append(next_state)
    print("Smoothing Done")
    return new_times_array, new_particles_array


def process(xyz, color_nearest_neighbors=False):
    # Array of elapsed times
    times = []
    # This is an array of arrays of Particles, which correspond to times
    particles = []
    n = 0
    for xyz_single in xyz.xyzs:
        # Particle array for this timestep
        pset = []
        times.append(float(xyz_single.comment))
        for i in range(xyz_single.number):
            particle = Particle()
            # Make sure the projectile atom always has id 0
            if i == 0:
                particle.id = 0
            particle.fromXYZ(xyz_single.atoms[i], xyz_single.values[i])
            pset.append(particle)
            n = n + 1
        particles.append(pset)
    merged = True
    print('Total: '+str(n))
    print('Merging Points')
    while merged:
        merged = False
        n = 0
        for i in range(len(particles)-1):
            pset1 = particles[i]
            pset2 = particles[i + 1]
            dt = times[i + 1] - times[i]
            didMerge, num = merge(pset1, pset2, dt, True, color_nearest_neighbors=color_nearest_neighbors)
            n = n + num
            merged = merged or didMerge
        if n == 0:
            print('Merged '+str(n))
            print('Number Per Frame: '+str(len(particles[0])))
            break
        for i in reversed(range(1, len(particles))):
            pset1 = particles[i]
            pset2 = particles[i - 1]
            dt = times[i - 1] - times[i]
            didMerge, num = merge(pset1, pset2, dt, False, color_nearest_neighbors=color_nearest_neighbors)
            n = n + num
            merged = merged or didMerge
        print('Merged '+str(n))
        print('Number Per Frame: '+str(len(particles[0])))
    print('Finished Merging')

    for pset in particles:
        def key(p):
            return p.id
        pset.sort(key=key)
    # Smooth the framerate
    times, particles = smooth_framerate(times, particles)
    xyz = XYZ()
    for i in range(len(times)):
        time = times[i]
        pset = particles[i]
        xyz_single = xyzFromParticles(time, pset)
        xyz.xyzs.append(xyz_single)
    return xyz


def process_file(fileIn, fileOut=None, color_nearest_neighbors=False, load_vmd=False):
    if fileOut is None:
        fileOut = fileIn
    xyz = XYZ()
    xyz.load(fileIn)
    xyz = process(xyz, color_nearest_neighbors=color_nearest_neighbors)
    if color_nearest_neighbors:
        fileOut = fileOut[:-4] + "COLORED.xyz"
    xyz.save(fileOut)
    if load_vmd:
        # MAKE THE FILENAME INCLUDE DIRECTORY
        if color_nearest_neighbors:
            commands = ["topo readvarxyz {}\n".format(fileOut)]
        else:
            commands = ["mol new {}\n".format(fileOut)]
        commands.append("mol modstyle 0 0 \"VDW\"")
        try :
            with open("commands.vmd", "w") as file:
                file.writelines(commands)
            os.system("vmd -e {}".format("commands.vmd"))
        finally:
            os.remove("commands.vmd")


if __name__ == '__main__':
    process_file('sample.xyz', 'sample_mod.xyz')
