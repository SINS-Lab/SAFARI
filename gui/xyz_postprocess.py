from xyz import XYZ
from xyz import XYZ_Single

class Particle:

    nextid = 0

    def __init__(self):
        self.atom = None
        self.pos = [0,0,0]
        self.momentum = [0,0,0]
        self.velocity = [0,0,0]
        self.mass = 0
        self.id = -1

    def fromXYZ(self, atom, value):
        self.atom = atom
        self.pos = [value[0], value[1], value[2]]
        self.momentum = [value[3], value[4], value[5]]
        self.mass = value[6]
        self.velocity = [value[3]/value[6], \
                         value[4]/value[6], \
                         value[5]/value[6]]

def isSame(p_a, p_b, dt):
    if p_a == p_b:
        return True
    if p_a.atom != p_b.atom:
        return False
    #always the same for the projectile Atom
    if p_a.atom == 'A':
        return True
    
    p_a_dt = [p_a.pos[0] + p_a.velocity[0]*dt,\
              p_a.pos[1] + p_a.velocity[1]*dt,\
              p_a.pos[2] + p_a.velocity[2]*dt]
    
    p_b_dt = [p_b.pos[0] + p_b.velocity[0]*dt,\
              p_b.pos[1] + p_b.velocity[1]*dt,\
              p_b.pos[2] + p_b.velocity[2]*dt]
    
    diff = (p_a_dt[0] - p_b_dt[0])**2+\
           (p_a_dt[1] - p_b_dt[1])**2+\
           (p_a_dt[2] - p_b_dt[2])**2
    return diff <= 1e-0

def merge(pset1, pset2, dt, first):
    len1 = len(pset1)
    len2 = len(pset2)
    merged = False
    num = 0
    for i in range(len1):
        p_a = pset1[i]
        has = False
        for j in range(len2):
            p_b = pset2[j]
            if isSame(p_a, p_b, dt):
                has = True
                if not first:
                    break
                if p_b.id != -1:
                    p_a.id = p_b.id
                elif p_a.id!=-1:
                    p_b.id = p_a.id
                else:
                    p_a.id = p_b.id = Particle.nextid
                    Particle.nextid = Particle.nextid + 1

        if not has:
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

def process(xyz):
    times = []
    particles = []
    n = 0
    for xyz_single in xyz.xyzs:
        pset = []
        times.append(float(xyz_single.comment))
        for i in range(xyz_single.number):
            particle = Particle()
            particle.fromXYZ(xyz_single.atoms[i],xyz_single.values[i])
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
            didMerge, num = merge(pset1, pset2, dt, True)
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
            didMerge, num = merge(pset1, pset2, dt, False)
            n = n + num
            merged = merged or didMerge
        print('Merged '+str(n))
        print('Number Per Frame: '+str(len(particles[0])))
    print('Finished Merging')
    xyz = XYZ()
    for pset in particles:
        def key(p):
            return p.id
        pset.sort(key=key)
    for i in range(len(times)):
        time = times[i]
        pset = particles[i]
        xyz_single = xyzFromParticles(time, pset)
        xyz.xyzs.append(xyz_single)
    return xyz

def process_file(fileIn, fileOut=None):
    if fileOut is None:
        fileOut = fileIn
    xyz = XYZ()
    xyz.load(fileIn)
    xyz = process(xyz)
    xyz.save(fileOut)


if __name__ == '__main__':
    process_file('sample.xyz', 'sample_mod.xyz')