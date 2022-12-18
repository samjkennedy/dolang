use predicates::prelude::*;

use std::fs::{self, OpenOptions};
use std::io::Write;
use std::process::Command;

use assert_cmd::prelude::{CommandCargoExt, OutputAssertExt, OutputOkExt};

#[test]
fn run_test_cases() {
    let record = false;

    let paths = fs::read_dir(".\\tests\\test_cases")
        .unwrap()
        .filter(|path| {
            path.as_ref().unwrap().file_type().is_ok()
                && path.as_ref().unwrap().file_type().unwrap().is_file()
        })
        .filter(|path| {
            &path.as_ref().unwrap().file_name().to_str().unwrap()
                [path.as_ref().unwrap().file_name().len() - 3..]
                == ".do"
        });

    for path in paths {
        println!("Running: {}", path.as_ref().unwrap().path().display());

        let expected_path = path.as_ref().unwrap().path().to_str().unwrap()
            [..path.as_ref().unwrap().path().to_str().unwrap().len() - 3]
            .to_owned()
            + "_expected.txt";

        let cmd = Command::cargo_bin("dolang")
            .unwrap()
            .arg(path.as_ref().unwrap().path().to_str().unwrap())
            .unwrap();

        let mut expected_file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&expected_path)
            .unwrap();

        if record {
            println!("Updating {}", &expected_path);
            expected_file.write_all(&cmd.stdout);
        } else {
            //TODO: If it fails dump the actual output to a file
            cmd.assert().stdout(predicate::eq(
                fs::read_to_string(&expected_path)
                    .expect("Should have been able to read the file")
                    .as_bytes(),
            ));
        }
    }
}
