import math
import numpy as np
import plotly.graph_objects as go

# ============================================================
# INPUT
# ============================================================

points = np.array([
  [0.000,  0.000,  0.000],
  [-0.831, 0.000,  0.000],
  [-0.831, 0.000,  1.046],
  [0.673,  0.000,  0.462],
  [2.588,  0.000,  0.346],
  [2.588, -1.443,  0.346],
  [0.673, -1.443,  0.462],
  [-0.992, -1.509, 0.835],
  [-0.941, -2.340, 0.915],
  [-0.738, -4.304, 1.643],
  [-0.473, -6.006, 2.851],
  [0.084,  -7.756, 3.690],
  [1.712,  -7.433, 3.514]

])

R = 0.425

# Number of points used to draw each bend
ARC_RESOLUTION = 40


# ============================================================
# HELPER FUNCTIONS
# ============================================================

def unit(v):
    mag = np.linalg.norm(v)

    if mag == 0:
        raise ValueError("Zero-length segment encountered.")

    return v / mag


def angle_between(a, b):
    value = np.clip(np.dot(unit(a), unit(b)), -1.0, 1.0)
    return math.acos(value)


# ============================================================
# GENERATE ROUNDED PATH
# ============================================================

path = []
bend_data = []

path.append(points[0])    # start at first point

for i in range(1, len(points) - 1):

    previous = points[i - 1]
    corner = points[i]
    next_point = points[i + 1]

    # Vectors pointing away from the corner
    u1 = unit(previous - corner)
    u2 = unit(next_point - corner)

    # Included angle between adjacent straight sections
    alpha = angle_between(u1, u2)

    # Deflection / bend angle
    delta = math.pi - alpha

    # Tangent setback from theoretical corner
    tangent = R * math.tan(delta / 2)

    incoming_length = np.linalg.norm(previous - corner)
    outgoing_length = np.linalg.norm(next_point - corner)

    if tangent >= incoming_length or tangent >= outgoing_length:
        print(
            f"WARNING: Bend at P{i} may not fit. "
            f"Tangent setback = {tangent:.4f}"
        )

    # Tangent points
    tangent_in = corner + u1 * tangent
    tangent_out = corner + u2 * tangent

    # Angle bisector
    bisector_vector = u1 + u2

    if np.linalg.norm(bisector_vector) < 1e-12:
        raise ValueError(
            f"P{i} is essentially straight and does not require a bend."
        )

    bisector = unit(bisector_vector)

    # Circle center
    center_distance = R / math.cos(delta / 2)
    center = corner + bisector * center_distance

    # Radius vectors
    r_start = tangent_in - center
    r_end = tangent_out - center

    # Plane normal
    normal = unit(np.cross(r_start, r_end))

    # Arc angle
    arc_angle = angle_between(r_start, r_end)

    # Add straight portion up to tangent point
    path.append(tangent_in)

    # Generate points along circular arc
    for theta in np.linspace(0, arc_angle, ARC_RESOLUTION)[1:]:

        r_rotated = (
            r_start * math.cos(theta)
            + np.cross(normal, r_start) * math.sin(theta)
            + normal
            * np.dot(normal, r_start)
            * (1 - math.cos(theta))
        )

        path.append(center + r_rotated)

    bend_data.append({
        "point": i,
        "angle": delta,
        "tangent": tangent,
        "arc_length": R * delta,
        "tangent_in": tangent_in,
        "tangent_out": tangent_out,
        "center": center
    })


path.append(points[-1])

path = np.array(path)


# ============================================================
# CALCULATE LENGTH
# ============================================================

sharp_length = sum(
    np.linalg.norm(points[i + 1] - points[i])
    for i in range(len(points) - 1)
)

removed_length = sum(
    2 * bend["tangent"]
    for bend in bend_data
)

total_arc_length = sum(
    bend["arc_length"]
    for bend in bend_data
)

finished_length = (
    sharp_length
    - removed_length
    + total_arc_length
)


# ============================================================
# PRINT RESULTS
# ============================================================

print()
print("=" * 60)
print("PATH LENGTH REPORT")
print("=" * 60)

print(f"Sharp-corner length:   {sharp_length:.6f}")
print(f"Straight removed:      {removed_length:.6f}")
print(f"Arc length added:      {total_arc_length:.6f}")
print(f"Finished path length:  {finished_length:.6f}")

print()
print("BENDS")
print("-" * 60)

for bend in bend_data:

    print(
        f"P{bend['point']} | "
        f"Angle = {math.degrees(bend['angle']):.3f} deg | "
        f"Setback = {bend['tangent']:.4f} | "
        f"Arc = {bend['arc_length']:.4f}"
    )


# ============================================================
# CREATE LABELS
# ============================================================

point_labels = [
    f"P{i}"
    for i in range(len(points))
]

hover_labels = [
    (
        f"<b>P{i}</b><br>"
        f"X = {p[0]:.3f}<br>"
        f"Y = {p[1]:.3f}<br>"
        f"Z = {p[2]:.3f}"
    )
    for i, p in enumerate(points)
]


# ============================================================
# CREATE 3D PLOT
# ============================================================

fig = go.Figure()


# ------------------------------------------------------------
# Actual finished centerline with radius bends
# ------------------------------------------------------------

fig.add_trace(
    go.Scatter3d(
        x=path[:, 0],
        y=path[:, 1],
        z=path[:, 2],

        mode="lines",

        line=dict(
            width=7
        ),

        name="Finished centerline",

        hoverinfo="skip"
    )
)


# ------------------------------------------------------------
# Original XYZ points
# ------------------------------------------------------------

fig.add_trace(
    go.Scatter3d(
        x=points[:, 0],
        y=points[:, 1],
        z=points[:, 2],

        mode="markers+text",

        text=point_labels,
        textposition="top center",

        hovertext=hover_labels,
        hoverinfo="text",

        marker=dict(
            size=6
        ),

        name="XYZ points"
    )
)


# ------------------------------------------------------------
# Sharp-corner reference path
# ------------------------------------------------------------

fig.add_trace(
    go.Scatter3d(
        x=points[:, 0],
        y=points[:, 1],
        z=points[:, 2],

        mode="lines",

        line=dict(
            width=2,
            dash="dash"
        ),

        name="Sharp reference path",

        hoverinfo="skip"
    )
)


# ============================================================
# PLOT SETTINGS
# ============================================================

fig.update_layout(

    title=(
        f"3D Assembly Centerline"
        f"<br>R = {R:.3f} | "
        f"Finished Length = {finished_length:.4f}"
    ),

    scene=dict(

        xaxis=dict(
            title="X"
        ),

        yaxis=dict(
            title="Y"
        ),

        zaxis=dict(
            title="Z"
        ),

        # Keeps X, Y, and Z physically proportional
        aspectmode="data"
    ),

    legend=dict(
        x=0.02,
        y=0.98
    ),

    margin=dict(
        l=0,
        r=0,
        b=0,
        t=80
    )
)


# ============================================================
# DISPLAY IN GOOGLE COLAB
# ============================================================

fig.show(renderer="colab")
