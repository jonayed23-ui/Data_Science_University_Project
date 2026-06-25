import warnings
import requests
from bs4 import BeautifulSoup, XMLParsedAsHTMLWarning
import pandas as pd
import time

warnings.filterwarnings("ignore", category=XMLParsedAsHTMLWarning)

COURT_API_TOKEN = "2a7c90755e1789049d11e37ba1d266d85b2efa39"

HEADERS = {
    "User-Agent": "Mozilla/5.0 (compatible; legal-dataset-collector/1.0)"
}

court_data    = []
contract_data = []
policy_data   = []
count = 1


def add_document(text, label, target_list):
    global count
    text = " ".join(text.split())
    if len(text) > 300:
        target_list.append({
            "number": count,
            "text":   text,
            "label":  label
        })
        count += 1


def strip_html(html_text):
    soup = BeautifulSoup(html_text, "lxml")
    return soup.get_text(separator=" ")


def collect_court(target=250):
    print("\n[1/3] Collecting court judgements...")

    if not COURT_API_TOKEN:
        print("  Set COURT_API_TOKEN at the top of this file.")
        return

    headers = {**HEADERS, "Authorization": f"Token {COURT_API_TOKEN}"}
    url = (
        "https://www.courtlistener.com/api/rest/v4/opinions/"
        "?page_size=50&order_by=-date_created"
    )

    while url and len(court_data) < target:
        try:
            res = requests.get(url, headers=headers, timeout=15)

            if res.status_code == 401:
                print("  Unauthorized - check COURT_API_TOKEN.")
                break
            if res.status_code != 200:
                print(f"  HTTP {res.status_code}")
                break

            data = res.json()
            if "results" not in data:
                print("  API issue:", data)
                break

            for item in data["results"]:
                if len(court_data) >= target:
                    break

                text = (
                    item.get("plain_text") or
                    item.get("html_with_citations") or
                    item.get("html") or
                    ""
                )

                if "<" in text:
                    text = strip_html(text)

                add_document(text, 1, court_data)

            print(f"  Collected: {len(court_data)} / {target}")
            url = data.get("next")
            time.sleep(1)

        except Exception as e:
            print(f"  Error: {e}")
            break

    print(f"  Court done: {len(court_data)} documents")


def _edgar_url_from_hit(hit):
    doc_id    = hit.get("_id", "")
    entity_id = hit.get("_source", {}).get("entity_id", "")
    if ":" not in doc_id or not entity_id:
        return None
    accession, filename = doc_id.split(":", 1)
    acc_nodash = accession.replace("-", "")
    return f"https://www.sec.gov/Archives/edgar/data/{entity_id}/{acc_nodash}/{filename}"


def _fetch_sec_text(url):
    res = requests.get(url, headers=HEADERS, timeout=20)
    soup = BeautifulSoup(res.text, "lxml")
    paragraphs = [p.get_text(separator=" ") for p in soup.find_all("p")]
    return " ".join(paragraphs)


def collect_contracts(target=250):
    print("\n[2/3] Collecting contracts...")

    filing_urls = []
    seen = set()

    search_queries = [
        "https://efts.sec.gov/LATEST/search-index?q=%22employment+agreement%22&forms=EX-10.1&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22employment+agreement%22&forms=EX-10.2&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22license+agreement%22&forms=EX-10.3&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22license+agreement%22&forms=EX-10.4&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22services+agreement%22&forms=EX-10.1&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22consulting+agreement%22&forms=EX-10.2&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22credit+agreement%22&forms=EX-10.1&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22loan+agreement%22&forms=EX-10.2&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22supply+agreement%22&forms=EX-10.3&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22lease+agreement%22&forms=EX-10.1&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22settlement+agreement%22&forms=EX-10.2&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
        "https://efts.sec.gov/LATEST/search-index?q=%22partnership+agreement%22&forms=EX-10.3&dateRange=custom&startdt=2022-01-01&enddt=2024-12-31",
    ]

    for q_url in search_queries:
        if len(filing_urls) >= target * 2:
            break
        try:
            res = requests.get(q_url, headers=HEADERS, timeout=15)
            hits = res.json().get("hits", {}).get("hits", [])
            for hit in hits:
                url = _edgar_url_from_hit(hit)
                if url and url not in seen:
                    seen.add(url)
                    filing_urls.append(url)
            time.sleep(0.5)
        except Exception as e:
            print(f"  EDGAR search error: {e}")

    print(f"  Phase A found {len(filing_urls)} contract URLs")

    for url in filing_urls:
        if len(contract_data) >= target:
            break
        try:
            text = _fetch_sec_text(url)
            add_document(text, 2, contract_data)
            print(f"  Collected: {len(contract_data)} / {target}")
            time.sleep(1.5)
        except Exception as e:
            print(f"  Scrape error: {e}")

    if len(contract_data) < target:
        print("  Phase B: EDGAR submissions API...")

        companies = [
            ("0000320193", "Apple"),
            ("0000789019", "Microsoft"),
            ("0001018724", "Amazon"),
            ("0001652044", "Alphabet"),
            ("0001326801", "Meta"),
            ("0001045810", "Nvidia"),
            ("0000051143", "IBM"),
            ("0001403161", "Visa"),
            ("0001800227", "Pfizer"),
            ("0000723254", "Intel"),
            ("0000012927", "Boeing"),
            ("0000040987", "General Motors"),
        ]

        for cik, name in companies:
            if len(contract_data) >= target:
                break
            try:
                sub_url = f"https://data.sec.gov/submissions/CIK{cik}.json"
                res = requests.get(sub_url, headers=HEADERS, timeout=10)
                filings = res.json().get("filings", {}).get("recent", {})

                forms      = filings.get("form", [])
                accessions = filings.get("accessionNumber", [])
                docs       = filings.get("primaryDocument", [])

                for form, accession, doc in zip(forms, accessions, docs):
                    if form not in ("8-K", "10-K") or len(contract_data) >= target:
                        continue

                    acc_nodash = accession.replace("-", "")
                    doc_url = (
                        f"https://www.sec.gov/Archives/edgar/data/"
                        f"{int(cik)}/{acc_nodash}/{doc}"
                    )

                    if doc_url in seen:
                        continue
                    seen.add(doc_url)

                    try:
                        text = _fetch_sec_text(doc_url)
                        add_document(text, 2, contract_data)
                        print(f"  [{name}] Collected: {len(contract_data)} / {target}")
                        time.sleep(1.5)
                    except Exception as e:
                        print(f"  Doc error ({name}): {e}")

            except Exception as e:
                print(f"  Submissions error ({name}): {e}")

    print(f"  Contracts done: {len(contract_data)} documents")


def _extract_policy_sections(soup):
    sections = []
    headers = soup.find_all(["h1", "h2", "h3"])

    if headers:
        for header in headers:
            parts = [header.get_text(separator=" ")]
            for sibling in header.next_siblings:
                if hasattr(sibling, "name") and sibling.name in ["h1", "h2", "h3"]:
                    break
                if hasattr(sibling, "get_text"):
                    part = sibling.get_text(separator=" ").strip()
                    if part:
                        parts.append(part)
            section_text = " ".join(parts)
            if len(section_text) > 300:
                sections.append(section_text)
    else:
        full = soup.get_text(separator=" ")
        if len(full.strip()) > 300:
            sections.append(full)

    return sections


def collect_policies(target=250):
    print("\n[3/3] Collecting policies...")

    policy_urls = [
        "https://policies.google.com/privacy",
        "https://www.microsoft.com/en-us/privacy/privacystatement",
        "https://openai.com/policies/privacy-policy",
        "https://www.apple.com/legal/privacy/en-ww/",
        "https://www.linkedin.com/legal/privacy-policy",
        "https://www.reddit.com/policies/privacy-policy",
        "https://zoom.us/privacy",
        "https://slack.com/trust/privacy/privacy-policy",
        "https://www.dropbox.com/privacy",
        "https://www.adobe.com/privacy/policy.html",
        "https://www.netflix.com/privacy",
        "https://www.paypal.com/us/legalhub/privacy-full",
        "https://www.uber.com/global/en/privacy/notice/",
        "https://twitter.com/en/privacy",
        "https://www.spotify.com/us/legal/privacy-policy/",
        "https://www.snapchat.com/privacy/privacy-policy/",
        "https://www.pinterest.com/legal/privacy-policy/",
        "https://www.intel.com/content/www/us/en/privacy/intel-privacy-notice.html",
        "https://www.cisco.com/c/en/us/about/legal/privacy-full.html",
        "https://www.salesforce.com/company/privacy/",
        "https://www.oracle.com/legal/privacy/privacy-policy.html",
        "https://policies.google.com/terms",
        "https://www.microsoft.com/en-us/servicesagreement",
        "https://openai.com/policies/terms-of-use",
        "https://www.apple.com/legal/internet-services/terms/site.html",
        "https://www.linkedin.com/legal/user-agreement",
        "https://www.reddit.com/policies/user-agreement",
        "https://www.adobe.com/legal/terms.html",
        "https://zoom.us/terms",
        "https://slack.com/terms-of-service",
        "https://www.dropbox.com/terms",
        "https://www.paypal.com/us/legalhub/useragreement-full",
        "https://www.spotify.com/us/legal/end-user-agreement/",
        "https://www.pinterest.com/legal/terms-of-service/",
        "https://www.salesforce.com/company/legal/sfdc-website-terms-of-service/",
        "https://www.irs.gov/privacy-disclosure/irs-privacy-policy",
        "https://www.ftc.gov/policy/privacy-policy",
        "https://www.sec.gov/privacy-act-statement",
        "https://www.fda.gov/about-fda/about-website/fda-website-policies",
        "https://www.hhs.gov/web/policies-and-standards/hhs-web-policies/privacy/index.html",
    ]

    for url in policy_urls:
        if len(policy_data) >= target:
            break
        try:
            res = requests.get(url, headers=HEADERS, timeout=15)
            soup = BeautifulSoup(res.text, "lxml")
            sections = _extract_policy_sections(soup)

            for section in sections:
                if len(policy_data) >= target:
                    break
                add_document(section, 3, policy_data)

            print(f"  [{len(sections)} sections] Total: {len(policy_data)} / {target} — {url[:55]}")
            time.sleep(2)

        except Exception as e:
            print(f"  Error ({url[:50]}...): {e}")

    print(f"  Policies done: {len(policy_data)} documents")


if __name__ == "__main__":
    collect_court(target=250)
    collect_contracts(target=250)
    collect_policies(target=250)

    all_data = court_data + contract_data + policy_data
    df = pd.DataFrame(all_data)
    df.drop_duplicates(subset="text", inplace=True)
    df = df.reset_index(drop=True)
    df["number"] = range(1, len(df) + 1)

    df.to_csv("legal_dataset.csv", index=False)

    print("\n========== DONE ==========")
    print(f"  Court judgements : {len(court_data)}")
    print(f"  Contracts        : {len(contract_data)}")
    print(f"  Policies         : {len(policy_data)}")
    print(f"  Total (deduped)  : {len(df)}")
    print(f"  Saved            : legal_dataset.csv")
    print()
    print("  Columns : number | text | label")
    print("  Labels  : 1 = court | 2 = contract | 3 = policy")