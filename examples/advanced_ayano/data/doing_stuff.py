import csv

# let's just create a bunch of variables and fill a local generated table with them
with open("calculations_table.csv", "w") as f:
    writer = csv.DictWriter(
        f,
        fieldnames=[
            "name",
            "value",
            "err",
            "%",
        ],
        quoting=csv.QUOTE_NONNUMERIC,
        delimiter=" ",
        quotechar="'",
    )
    writer.writeheader()

    tau = (4.3535223, 0.0034342)
    pi = (4334.34434, 10)
    chi = (-2332.332424, 1e-7)
    zeta = (4444.00, 0.033)

    def write_var(writer, val, name):
        writer.writerow(
            {
                "name": name,
                "value": val[0],
                "err": val[1],
                "%": round(val[1] / val[0] / 100, ndigits=6),
            }
        )

    write_var(writer, tau, 'tau')
    write_var(writer, pi, 'pi')
    write_var(writer, chi, 'chi')
    write_var(writer, zeta, 'zeta')

# say, we do something undesirable in this script next, and we do not want kyomato to execute that
# TOSHINO KYOKO! <-- this terminates the execution

exit(-42)